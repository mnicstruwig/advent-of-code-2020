(ns aoc
  (:require [clojure.string :as str]))

(def sample-input (str/split-lines "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"))

(def possible-keys ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"])

;; PART 1
(defn chunk-input
  "Parse the raw input lines into passport chunks."
  [input-lines]
  (map #(str/join " " %)
     (filter
      #(not (str/blank? (first %))) (partition-by str/blank? input-lines))))

(defn make-mapped-passport
  "Map a raw chunked passport into a map"
  [raw-passport]
  (zipmap possible-keys (map str/includes? (cycle [raw-passport]) possible-keys))
  )

(defn is-valid?
  "Takes a single chunked input and checks if it's valid."
  [chunked-input]
  (every? true? (map (make-mapped-passport chunked-input) (filter #(not= "cid" %) possible-keys))))

(def puzzle-input (str/split-lines (slurp "input_004.txt")))
(count (filter true? (map is-valid? (chunk-input puzzle-input)))) ; => 233


;; PART 2

(def puzzle-inputs-valid
  "Get the valid passports from Part 1."
  (filter is-valid? (chunk-input puzzle-input)))

(defn split-key-values
  [chunked-input]
  (map #(str/split % #":") (str/split chunked-input #" ")))

(defn make-mapped-values-passport
  [chunked-passport]
  (let [key-value-pairs (split-key-values chunked-passport)]
    (reduce  ; Black magic -- build a map from key-value pairs
     (fn [coll [key value]] (assoc coll key value)) {} key-value-pairs)))

(defn in-range?
  "Return true if `min <= val <= max`."
  [val, min, max]
  (let [val (Integer/parseInt val)]
    (and (>= val min) (<= val max))))

(defn valid-hgt?
  "Is the height valid?"
  [hgt-str]
  (cond
    (str/includes? hgt-str "cm") (in-range? (str/replace hgt-str "cm" "") 150 193)
    (str/includes? hgt-str "in") (in-range? (str/replace hgt-str "in" "") 59 76)))

(defn valid-hcl?
  [hcl-str]
  (some? (re-matches #"#[1-9a-f]{6}" hcl-str)))

(defn valid-ecl?
  [ecl-str]
  (some? (some #{ecl-str} '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))))

(defn valid-pid?
  [pid-str]
  (some? (re-matches #"[0-9]{9}" pid-str)))


(def rules {"byr" #(in-range? % 1920 2020)
            "iyr" #(in-range? % 2010 2020)
            "eyr" #(in-range? % 2020 2030)
            "hgt" valid-hgt?
            "hcl" valid-hcl?
            "ecl" valid-ecl?
            "pid" valid-pid?}) ; More to come...

(defn apply-rules
  [coll [k func]]
  (func (coll k)))

(defn are-passport-values-valid?
  [mapped-values-passport]
  (every? true?
          (map apply-rules
               (repeat mapped-values-passport)
               rules)))

(count (filter true?
               (map are-passport-values-valid?
                    (map make-mapped-values-passport puzzle-inputs-valid)))) ; => TBD
