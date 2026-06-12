(ns main
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [lexer :refer [tokenize-file]]
            [syntax-checker :refer [syntax-check]]
            [parser :refer [parse-program]]
            [html-generator :as html-generator]))

;; Merged execution pipeline function
(defn process-file [file-path]
  (let [output-path (str (str/replace file-path #"\.py$" "") ".html")]
    (syntax-check file-path)
    (html-generator/run file-path output-path) ;; Generates HTML and clears linter warnings
    (parse-program file-path)))

(defn is-python-file? [file-object]
  (str/ends-with? (.getName file-object) ".py"))

(defn process-directory-sequential [file-paths]
  (mapv process-file file-paths))

(defn process-directory-parallel-futures [file-paths]
  (let [active-tasks (mapv (fn [path] (future (process-file path))) file-paths)]
    (mapv deref active-tasks)))

(defn run-benchmarks [folder-path]
  (let [directory-object (io/file folder-path)]

    (when (.isDirectory directory-object)
      (let [all-files-and-folders (file-seq directory-object)
            python-files (filter is-python-file? all-files-and-folders)
            file-paths (mapv (fn [f] (.getPath f)) python-files)]

        (if (empty? file-paths)
          (println "No Python files found in this directory.")

          ;; Single run benchmark logic
          (let [start-seq (System/nanoTime)
                _ (process-directory-sequential file-paths)
                end-seq (System/nanoTime)
                seq-seconds (/ (- end-seq start-seq) 1e9)

                start-par (System/nanoTime)
                _ (process-directory-parallel-futures file-paths)
                end-par (System/nanoTime)
                par-seconds (/ (- end-par start-par) 1e9)
                speedup-factor (/ seq-seconds par-seconds)]

            (println (str "Sequential Execution Time: " seq-seconds " seconds"))
            (println (str "Parallel Future Time:      " par-seconds " seconds"))
            (println (str "Calculated Speedup:        " speedup-factor "x faster"))))))))

(defn -main [& arguments]
  (if (empty? arguments)
    (println "not good")
    (let [target-folder (first arguments)]
      (run-benchmarks target-folder))))

(-main)
