(ns advent-of-code-2020.day-1
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.math.combinatorics :refer [combinations]]
            [criterium.core :refer [quick-bench]]))

;; # Day 1: Report Repair (https://adventofcode.com/2020/day/1)
;;
;; Find two entries in the input that sum to 2020, and multiply those
;; numbers together.

(def input
  "Puzzle inputs vary by user so you have to be authenticated and can't
  just fetch them on demand, so let's just cut and paste."
  [1822 1917 1642 1617 1941 1740 1529 1896 1880 568 1897 1521 1832 1936 611 1475 1950 1895 1532 1721 1498 1905 1770 1845 2003 1854 1705 1916 1913 1956 1798 1823 1955 1713 1942 1710 1696 1590 1966 1476 1800 1672 1533 1524 1957 1923 1545 534 1707 1760 1104 1471 1947 1802 1525 1931 1653 1608 1937 1977 1598 1470 1794 1488 1786 1652 1482 1603 1667 1245 1478 667 1948 1885 547 1971 1795 1910 1571 1711 1727 1987 1597 1586 1661 1893 1873 1827 1561 2006 1782 1813 2000 1592 1714 1849 1501 1809 1751 1935 1692 1697 1878 1502 1738 1731 1682 1690 1499 1641 1925 1996 1972 1886 1836 1747 1841 1668 715 1698 1859 1637 1477 1785 1695 1702 1944 1631 1771 1623 1892 1466 1834 1899 201 1801 1978 1830 1591 1673 1949 1846 1677 1657 1576 1817 1851 1894 1754 1604 1568 1730 1985 1614 1980 1554 1997 1960 1983 1848 1883 1968 1729 1716 628 1472 1676 1943 1821 1681 1619 1644 842 1492 1633 1921 775 1861 1584 1974 585 1898 1560 1708 1927 1563 1872 1876 1865 1535 1994 1756 1662 1621 1993 1825 1679 1959 1691 1875])

(defn naive-solution
  "The naive solution is just grab every combination of `n` items and
  test them all until we find one that sums to 2020."
  ([input n]
   (first (for [xs (combinations input n)
                :let [sum (apply + xs)]
                :when (= sum 2020)]
            (apply * xs))))
  ([input]
   (naive-solution input 2)))

(defn recursive-solution
  "The simplest recursive solution short circuits a bit more and so is a
  little faster with bigger inputs and subsets."
  ([input n sum product]
   (cond
     (and (zero? n)
          (= sum 2020)) product
     (or (zero? n)
         (> sum 2020)) nil
     :else (some identity
                 (map-indexed
                  (fn [i x]
                    (recursive-solution (concat (take i input)
                                                (drop (inc i) input))
                                        (dec n)
                                        (+ sum x)
                                        (* product x)))
                  input))))
  ([input n]
   (recursive-solution input n 0 1))
  ([input]
   (recursive-solution input 2)))

;; ## Testing
;;
;; Some test cases. It's important that we don't allow edge cases like
;; summing a number with itself.

(deftest solution-tests
  (doseq [solution [naive-solution recursive-solution]]
    (testing (clojure.repl/demunge (str solution))
      (testing "doesn't find the sum if list is too short"
        (is (nil? (solution [1010]))))
      (testing "doesn't allow numbers to sum with themselves"
        (is (nil? (solution [1010 1 2 3]))))
      (testing "returns correct product for valid lists"
        (is (= (* 1010 1010) (solution [1010 1010])))
        (is (= 514579 (solution [1721 979 366 299 675 1456])))
        (is (= 964875 (solution input))))
      (testing "allows combinations of arbitrary size"
        (is (= 241861950 (solution [1721 979 366 299 675 1456] 3)))
        (is (= 158661360 (solution input 3)))))))

(defn random-input
  "Generate a test sequence guaranteed to contain a subset with length
  `n` that sums to 2020."
  [length n]
  (let [[xs rest] (->> (partial rand-int 2100)
                       repeatedly
                       (take (dec length))
                       (split-at (dec n)))
        ;; guarantee at least one hit
        y (apply - 2020 xs)]
    (shuffle (concat [y] xs rest))))

;; # Benchmarks
;;
;; Comparing the performance of the naive and recursive
;; approaches. Recorded on a Linux VM running on a Chromebook, please
;; don't laugh.

;; Evaluation count : 6 in 6 samples of 1 calls.
;; Execution time mean : 118.488980 ms
;; Execution time std-deviation : 30.464186 ms
;; Execution time lower quantile : 81.965276 ms ( 2.5%)
;; Execution time upper quantile : 148.631309 ms (97.5%)
;; Overhead used : 31.894745 ns

(comment (quick-bench (recursive-solution (random-input 10000 3) 3)))

;; Evaluation count : 12 in 6 samples of 2 calls.
;; Execution time mean : 4.233666 sec
;; Execution time std-deviation : 9.750836 sec
;; Execution time lower quantile : 61.125691 ms ( 2.5%)
;; Execution time upper quantile : 21.115343 sec (97.5%)
;; Overhead used : 31.894745 ns

(comment (quick-bench (naive-solution (random-input 100000 3) 3)))
