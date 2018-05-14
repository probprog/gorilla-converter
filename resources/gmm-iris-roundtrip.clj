;; gorilla-repl.fileformat = 1

;; **
;;; # Gaussian mixture model: Iris data
;;; 
;;; Let's experiment with implementing a Gaussian mixture model in Anglican.
;; **

;; @@
(ns gmm-iris
  (:use [anglican core runtime emit 
         [state :only [get-predicts]]
         [inference :only [collect-by]]]
        [plotly-clj.core])
  (:require [gorilla-plot.core :as plot]
            [gorilla-repl.html :as html]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.core.matrix :as m
             :refer [matrix diagonal-matrix to-nested-vectors div ecount]]
            [clojure.core.matrix.operators :as op]
            [clojure.core.matrix.linear :as linalg]
            [anglican.stat :as stat]))

(m/set-current-implementation :vectorz)

(offline-init)

;; experimental support for hypothes.is
(html/html-view "<script src=\"https://hypothes.is/embed.js\" async></script>")
;; @@

;; **
;;; The data we will use is the [Fisher Iris data](https://archive.ics.uci.edu/ml/datasets/Iris);
;;; each row of data consists of four measurements made regarding an Iris flower.
;;; We will cluster the measurements using a Gaussian mixture model.
;;; 
;;; We begin by reading in the Iris data set from its CSV file, placing the measuments into a matrix $X$ and the labels into a vector $y$.
;; **

;; @@
(def iris-data
  (loop [X []
         y []
         data (with-open [file (io/reader "data/iris.csv")]
                (doall (csv/read-csv file)))]
    (def row (first data))
    (if (nil? row)
      [(m/matrix X) y]
      (recur (conj X (map #(Float/parseFloat %) (take 4 row)))
             (conj y (keyword (peek row)))
             (rest data)))))

(def X (get iris-data 0))
(def y (get iris-data 1))
;; @@

;; **
;;; Let's plot the Iris data to get an idea what it looks like. The data is 4-dimensional (i.e., 4 measurements per flower), but we can use PCA to project it down into 2 for visualization purposes.
;; **

;; @@
(comment ;; previous custom gorilla plot code

(def label-indices 
  (apply zipmap ((fn [v] 
                   [(into () v) 
                    (range (count v))])
                 (into #{} y))))

(def colors ["#34C6F2" "#64F54A" "#F13F6F" "#E4AEFB" "#253B3E" 
             "#8B5904" "#BBC324" "#EAC092" "#360A07" "#0888A4"])

(defn get-color [label]
  (get colors (label label-indices)))

(defn black [label]  "#000000")

(defn plot-2d [data labels color-fn & [jitter & options]]
  "plotting function that plots the first two 
  dimensions of the provided data"
  (reduce plot/compose
          (cons
            (plot/list-plot 
              (map #(take 2 %) data) :color "#ffffff")
            (for [[label indices] 
                  (group-by #(get labels %) 
                            (range (count labels)))]
              (let [points (m/select data indices [0 1])
                    color (color-fn label)
                    points (if jitter
                             (op/+ points 
                                   (op/* jitter 
                                         (sample* 
                                           (mvn [0. 0.] 
                                                (m/identity-matrix 2)))))
                             (op/+ points 0.0))]
                (plot/list-plot (m/to-nested-vectors points)
                                :color color :symbol-size 20))))))

(defn plot-pca [data labels color-fn]
  "plotting function that takes high-dimensional 
  data and projects into 2d for visualzation"
  (let [projection (m/transpose 
                     (m/select 
                       (:V* 
                         (linalg/svd 
                           X 
                           {:return [:V*]})) 
                       [1 0] 
                       :all))
        data (m/mmul data projection)]
    (plot-2d data labels color-fn))))
;; @@

;; @@
;; following http://viewer.gorilla-repl.org/view.html?source=github&user=findmyway&repo=plotly-clj&path=examples/scatter.clj

(defn plot-2d [data labels & [title]]
  "plotting function that plots the first two 
  dimensions of the provided data"
    (-> (reduce (fn [p [points label]]
                    (add-scatter p :x (mapv first points)
                                   :y (mapv second points)
                                   :name label
                                   :mode "markers" ))
            (plotly)
            (for [[label indices] 
                  (group-by #(get labels %) 
                            (range (count labels)))]
              (let [points (m/select data indices [0 1])]
                [(m/to-nested-vectors points) label]))
            )
        (set-layout :xaxis {:zeroline false}
                  :yaxis {:zeroline false}
                  :title title)
        iplot))

(defn plot-pca [data labels & [title]]
  "plotting function that takes high-dimensional 
  data and projects into 2d for visualzation"
  (let [projection (m/transpose 
                     (m/select 
                       (:V* 
                         (linalg/svd 
                           X 
                           {:return [:V*]})) 
                       [1 0] 
                       :all))
        data (m/mmul data projection)]
    (plot-2d data labels title)))
;; @@

;; **
;;; Compare the two resulting plots below. The first plots the first two dimensions of the data; the second plots the data projected onto the first two principal components.
;;; 
;;; It is clear that one of the Iris species is easily separable from the other two, while two of the varieties are less distinguishable.
;; **

;; @@
(plot-2d X y "First two dimensions" #_get-color #_0.01)
(plot-pca X y "PCA projection" #_get-color)
;; @@

;; **
;;; ## Unsupervised learning
;;; 
;;; In the plots above, we see the Iris data colored using the *true labels* given by $y$.
;;; One typical machine larning task would involve learning a classifier, which would allow us to predict the Iris variety of a particular flower (i.e., $y_n$) given only its measurements $\mathbf{x}_n$.
;;; 
;;; As alternative, we could consider an *unsupervised learning* task, where we suppose that the true Iris species labels were unknown. 
;; **

;; @@
(plot-pca X (vec (repeat (count y) :unknown)) "Unknown labels" #_black)
;; @@

;; **
;;; "In such a situation, given just the measurement data $\\{ \mathbf{x}_n \\}$, we may wish to ask questions such as 
;;; 
;;; * "Are two different measurements $\mathbf{x}_i$, $\mathbf{x}_j$ of the same or different Iris variety?"
;;; * "In the 150 total different measurements, how many different species of flowers might there be?"
;;; 
;;; Notice that these are qualitatively different tasks than _label prediction_ that we may attempt using (say) logistic regression, and labeled training data.
;;; 
;;; 
;;; ### Defining the model
;;; 
;;; We will use a Bayesian mixture of Gaussians; the model we use here is as described in section 10.2 of [Bishop 2006].
;;; The mixture of Gaussians supposes that the data $\\{ \mathbf{x}_n \\}$ can be modeled by a mixture of $K$ different Gaussian distributions, each with a mean $\boldsymbol{\mu}\_k$ and a covariance matrix $\boldsymbol{\Sigma}_k$. Each data point $\mathbf{x}\_n$ is assigned to one of the clusters $k = 1,\dots,K$ via a latent class indicator variable $z_n$. The probability of any particular measurement being assigned to some class $k$ is given by a component-wise probability $\pi_k$.
;;; 
;;; <!---The parameters of the mixture of Gaussians are $\boldsymbol{\pi}$, a vector of length $K$ containing-->
;;; In a Bayesian mixture model, we place _prior distributions_ on the parameters of the model,
;;; 
;;; $$\boldsymbol{\pi} \sim \mathrm{Dirichlet}(\alpha)$$
;;; $$\boldsymbol{\Lambda}_k \sim \mathrm{Wishart}(\mathbf{\Lambda}_0, 
;;; u)$$
;;; $$\boldsymbol{\mu}_k \big|\ \boldsymbol{\Lambda}_k \sim \mathrm{Normal}\hspace{-0.2em}\left(\mathbf{0}, (\beta \boldsymbol{\Lambda}_k)^{-1} \right)$$
;;; 
;;; and treat them as latent variables. Here we have parameratized the multivariate normal distribution with a *precision matrix* $\boldsymbol{\Lambda} \equiv \boldsymbol{\Sigma}^{-1}$, or inverse covariance matrix.
;;; The priors in this model are specifically chosen so as to be [conjugate to the likelihood](http://en.wikipedia.org/wiki/Conjugate_prior).
;;; 
;;; The likelihood of the data is defined as
;;; 
;;; $$z_n \big|\ \boldsymbol{\pi} \sim \mathrm{Categorical}(\boldsymbol{\pi})$$
;;; $$\mathbf{x}_n \big|\ z_n=k, \ \boldsymbol{\mu}_k, \ \boldsymbol{\Lambda}_k \sim \textrm{Normal}\hspace{-0.2em}\left(\ \boldsymbol{\mu}_k, \boldsymbol{\Lambda}_k^{-1} \right).$$
;;; 
;;; We can visualize the conditional independence structure as a graphical model:
;;; 
;;; <center>
;;; <p><img alt="Graphical model for Bayesian GMM" src="http://www.robots.ox.ac.uk/~fwood/teaching/AIMS_CDT_ML/homework/HW_3_sampling/graphical-models-crop.png" width="370px" style="padding-right: 20px" /></p>
;;; </center>
;;; 
;;; <!---We are going to perform inference using both MCMC and SMC methods.
;;; In both cases, we can take advantage of conjugacy to reduce the dimensionaliy of the
;;; space on which we need to sample.--->
;;; 
;;; We define this model as an Anglican program, conditioning on the Iris data, in the following cell.
;; **

;; @@
;; helper methods
(defn row-mean [data] (op// (reduce op/+ data) (m/row-count data)))
(defn invert
  ([W] (linalg/solve W))
  ([kappa W] (linalg/solve (op/* kappa W))))


;; bring required primitive procedures into our namespace
(use '[clojure.core.matrix :only [shape identity-matrix get-row]])


;; model definition
(with-primitive-procedures [row-mean invert shape identity-matrix get-row]
  (defquery gmm [data & [hyperparams]]
    (println "provided hyperparameters:" hyperparams)
    (let [[N D] (shape data)

          ;; there are many hyperparameters; we provide defaults
          K 		(:K 		hyperparams	10)
          alpha 	(:alpha		hyperparams	1.0)
          mu-0		(:mu-0		hyperparams (row-mean data))
          lambda-0 	(:lambda-0	hyperparams	(identity-matrix D))
          nu		(:nu		hyperparams	(inc D))
          kappa		(:kappa		hyperparams 1.0)

          ;; sample the latent variables.
          ;;
          ;; mu and sigma are per-cluster; ideally we would 
          ;; sample them lazily
          
          pi (sample (dirichlet (repeat K alpha)))
          lambda (into [] (map (fn [x] (sample x))
                               (repeat K (wishart nu lambda-0))))
          mu (into [] (map 
                        (fn [k] 
                          (sample (mvn mu-0 
                                       (invert kappa 
                                               (get lambda k)))))
                        (range K)))
          sigma (into [] (map invert lambda))]
      ;; for each data point, sample z[n] and `observe`
      (loop [n 0
             z []]
        (if (= n N)
          z
          (let [row (get-row data n)
                k (sample (discrete pi))]
            (observe (mvn (get mu k) (get sigma k)) row)
            (recur (inc n) (conj z k))))))))
;; @@

;; **
;;; Now, we draw some samples and plot them.
;; **

;; @@
(defn draw-samples [n-samples]
  "simple helper to draw samples"
  (take n-samples 
        (doquery :smc gmm [X {:K 5 
                              :mu-0 (row-mean X) 
                              :kappa 1.0 
                              :nu 10.0 
                              :lambda-0 (m/mul (m/identity-matrix 4) 0.5)
                              :alpha 0.5}] :number-of-particles 10000)))

;; draw samples; 
(def N 20000)
(def samples (draw-samples N))
;; @@

;; @@
(def sorted-importance-weighted-posterior-samples
  (->> samples
     (take N)
     (collect-by :result)
     (stat/empirical-distribution)
     (#(into (sorted-map-by (fn [key1 key2]
                         (compare [(get % key2) key2]
                                  [(get % key1) key1]))) %))))
;; @@

;; **
;;; Let's take a look at a cross-section of the results
;; **

;; @@
(->> (take-nth (/ N 50) sorted-importance-weighted-posterior-samples)
     (into {})
     (map key)
     (map (fn [y] (plot-pca X y #_(get colors %)))))
;; @@

;; **
;;; Notice that the labels may switch around, as the sampler explores different possible generative explanations of the data.
;;; 
;;; Looking at a histogram over the class assignment variables, we see a tendency towards three similarly-sized clusters (which, in fact, roughly correspond to the "true" Iris species labels, even though we did not even look at the labels themselves in this model!).
;; **

;; @@
(defn plot-histogram [x]
  (-> (plotly)
      (add-histogram :x x)
      iplot))
;; @@

;; @@
(->> (take-nth (/ N 50) sorted-importance-weighted-posterior-samples)
     (into {})
     (map key)
     (map (fn [z] (plot-histogram z))))
;; @@

;; **
;;; What if we want to check whether the first entry $\mathbf{x}_1$ is the same type of flower as $\mathbf{x}_2$?
;;; 
;;; We can compare our prediction to the ground truth by looking at the actual species labels in $y_1, y_2$.
;; **

;; @@
(defn are-x1-x2-the-same [z] (if (= (get z 1) (get z 2)) 1.0 0.0))

(defn check-same-cluster-pct [test-fn samples]
  (* 100
     (/ (reduce + (map #(test-fn (:result %)) samples))
        (count samples))))


(println "percent of samples in which x1 and x2 are in the same cluster:" 
         (check-same-cluster-pct are-x1-x2-the-same samples))

(println "
true labels for x1, x2:" (get y 1) (get y 2))
;; @@

;; **
;;; We can also try comparing two flowers we know to be in different classes, e.g. $\mathbf{x}\_1$ and $\mathbf{x}_{101}$, and see they are much less often included in the same cluster.
;; **

;; @@
(defn are-x1-x101-the-same [z] (if (= (get z 1) (get z 101)) 1.0 0.0))

(println "percent of samples in which x1 and x101 are in the same cluster:" 
         (check-same-cluster-pct are-x1-x101-the-same samples))

(println "
true labels for x1, x101:" (get y 1) (get y 101))
;; @@

;; **
;;; # Collapsed Representation
;; **

;; **
;;; We now consider a parametrization in terms of an *inverse-Wishart* prior:
;;; 
;;; $$\boldsymbol{\Sigma}_k \sim \mathrm{inverse\text{-}Wishart}(\mathbf{\Psi}, 
;;; u),$$
;;; $$\boldsymbol{\mu}_k \big|\ \boldsymbol{\Sigma}_k \sim \mathrm{Normal}\hspace{-0.2em}\left(\boldsymbol{\mu}_0, \frac{1}{\lambda} \boldsymbol{\Sigma}_k \right),$$
;;; 
;;; Instead of a *precision matrix* $\boldsymbol{\Lambda}$, this model samples a *covariance matrix* $\boldsymbol{\Sigma} \equiv \boldsymbol{\Lambda}^{-1}$. We also use the letter $\lambda$ instead of the letter $\beta = \lambda$, in order to be consistent with the definition of the <a href="https://en.wikipedia.org/wiki/Normal-inverse-Wishart_distribution">Normal-inverse-Wishart distribution</a> in Wikipedia.
;;; 
;;; The prior distributions over hyper-parameters $\boldsymbol{\mu}_0, \lambda, \mathbf{\Psi}, 
;;; u$ in this model are [conjugate](http://en.wikipedia.org/wiki/Conjugate_prior) to the observations $\mathbf{x}_n$. This means that the prior distribution $p(\boldsymbol{\mu}_0, \lambda, \mathbf{\Psi}, 
;;; u)$ over hyper-parameters for any cluster $k$ is in the same Normal-inverse-Wishart distribution family as the posterior distribution over hyper-parameters $p(\boldsymbol{\mu}_k{'}, \lambda_k{'}, \mathbf{\Psi}_k{'}, 
;;; u_k{'})$ for the cluster $k$ given emissions $\\{ x_i \text{ such that } z_i = k \\} $.
;;; 
;;; Thanks to conjugacy, given the hyper-priors of the Normal-inverse-Wishart distribution $\boldsymbol{\mu}_0, \lambda, \mathbf{\Psi}, 
;;; u$ and the emission values of the Gaussian emission distributions $\mathbf{x}_i$, the posterior hyper-priors of the posterior Normal-inverse-Wishart distribution can be calculated <a href="https://en.wikipedia.org/wiki/Conjugate_prior#Continuous_distributions">as follows</a>:
;;; 
;;; $$\boldsymbol{\mu}{'} = \frac{\lambda\boldsymbol\mu_0+n\mathbf{\bar{x}}}{\lambda+n},$$
;;; $$\lambda{'} = \lambda + n,$$
;;; $$
;;; u{'} = 
;;; u+n,$$
;;; $$\boldsymbol\Psi{'} = \boldsymbol\Psi + \mathbf{C} + \frac{\lambda n}{\lambda+n}(\mathbf{\bar{x}}-\boldsymbol\mu_0)(\mathbf{\bar{x}}-\boldsymbol\mu_0)^T,$$
;; **

;; @@
(with-primitive-procedures 
  [div ecount matrix diagonal-matrix to-nested-vectors]

  (defm mvn-mixture 
    [data obs-proc comp-proc]
    (loop [data data
           z []
           obs-procs {}
           comp-proc comp-proc]
      (let [y (first data)]
        (if y
          (let [k (sample (produce comp-proc))
                obs-proc (get obs-procs k obs-proc)
                obs-dist (produce obs-proc)]
            (observe obs-dist y)
            (recur (rest data)
                   (conj z k)
                   (assoc obs-procs
                     k (absorb obs-proc y))
                   (absorb comp-proc k)))
          z))))

  (defquery crp-mvn-mixture
    [data mu kappa nu psi alpha]
    (mvn-mixture data 
                 (mvn-niw mu kappa nu psi) 
                 (CRP alpha)))

  (defquery dir-mvn-mixture
    [data mu kappa nu psi alphas]
    (mvn-mixture data 
                 (mvn-niw mu kappa nu psi) 
                 (dirichlet-discrete alphas))))
;; @@

;; @@
(def shuffled-X (shuffle (m/to-nested-vectors X)))

(def samples 
  (->> (doquery :smc 
                dir-mvn-mixture
                [shuffled-X
                 (row-mean X) 0.5 10.0 (m/mul (m/identity-matrix 4) 0.5)
                 (into [] (repeat 5 1.0))]
                :number-of-particles 10000)
       (take 1000)
       doall
       time))
;; @@

;; @@
;; plot all unique sets of clusterings
(map (fn [z]
		(plot-pca shuffled-X z #_(get colors %)))
     (take-nth (/ 1000 200) (keys (collect-by :result samples))))
;; @@

;; @@
(->> (collect-by :result samples)
    keys
    (map frequencies))
;; @@

;; @@
(def samples 
  (->> (doquery :smc 
                crp-mvn-mixture
                [shuffled-X
                 (row-mean X) 0.5 10.0 (m/mul (m/identity-matrix 4) 0.5)
                 1.0]
                :number-of-particles 10000)
       (take 1000)
       doall
       time))
;; @@

;; @@
;; plot a subset of unique sets of clusterings
(map (fn [z]
		(plot-pca shuffled-X z #_(get colors %)))
     (take-nth (/ 1000 200) (keys (collect-by :result samples))))
;; @@

;; @@
(->> (collect-by :result samples)
    keys
    (map frequencies))
;; @@

;; @@

;; @@

