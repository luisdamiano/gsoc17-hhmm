# Personal logs #
Like astronauts do in sci-fi films, during the research process I will try to log my work often. Entries include facts and results, as well as guesses, expectations, worries and frustrations. Make sure you check the logs to meet the humans behind the code. Time spans are rough approximations expressed in ART (UTC-3).

---

# Stage I: Buildilding the environment setup #

### 20170404 Tu 22 to 24 ###
* Downloaded all the papers cited in the selected works.

### 20170405 W 21 to 24 ###
* Googled "hidden markov models stock market" and downloaded every single available paper. Will need to classify them later.
* I'll note a few packages, may be useful to see other implementations later (from: http://quant.stackexchange.com/a/21295/8149).
  * __HiddenMarkov__ when the distributions generating the parameters are continuous (probability density functions).
  * __HMM__ when the distributions are discrete (probability mass functions).
* Prepared folder structure

* Started setting up the research environment:
  * Installed R 3.3.3 from scratch (no packages).
  * Installed RStudio Desktop 1.0.136.
  * Installed Rtools 3.3 (R 3.2.x to 3.3.x) Frozen from https://cran.r-project.org/bin/windows/Rtools/
  * Installed Stan 2.14 and rstan 2.14.2
    following https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows
    
  > install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
  >
  > library("rstan")
  >
  > > rstan (Version 2.14.2, packaged: 2017-03-19 00:42:29 UTC, GitRev: 5fa1e80eb817)

### 20170406 Th 22 to 24 ###
* Read Pro Git book Chapters 1 and 2

### 20170407 F 23 to 24 ###
* Read Pro Git book Chapter 3, will have to review a few things later.

### 20170408 Sa 12 to 17
* Went on setting up the research environment:
  * Installed Git 2.12.2 64 bits for Windows (from https://git-scm.com/).
    - [x] Use Git from the Windows Command Prompt
    - [x] Use OpenSSH (comes with the Git)
    - [x] Use the OpenSSL library
    - [x] Checkout Windows-style, commit Unix-style line endings (recommended setting on Windows for cross platform projects)
    - [x] Use MinTTY (the default terminal of MSYS2)
    - [x] Enable file system caching
    - [x] Enable Git Credential Manager
    - [ ] Enable symbolic links.
  * Installed MiKTeX Portable 2.9.6236 32-bit from https://miktex.org/download
  * Installed JabRef 3.8.2 64-bit (Windows installer from https://www.fosshub.com/JabRef.html)
  * Dealt with RStudio settings
  * Started with README (the file, not the book!). Pending items:
    * Contributing
    * Versioning
    * License (which of the OSIs?)

* Started writing the literature review paper
  * Created folder and file structures.
  * Picked Murphy (2012) for a general overview.
    * Started with Chapters 10 on  graphical models (Bayesian nets). There's a terminology section that may be useful if we want to dig into Bayesian networks as a genereal framework for HMM and HHMM, even if it's for a brief intro. Even if not included in the final report, this will help to be precise in our work.
    * Will go on with Chapter 17 on HMM later.

---

# Stage II: Literature review #

### 20170409 Su 13 to 19 ###
* Added Koller (2009)'s Probabilistic Graphical Models Principles and Techniques to our resources list. It covers inference algos in details, may be a reference of help for the coding stage.

* Went on with Murphy (2012)


* Chapter 10
    * __Sections most related to our work__: 10.1 (intro), 10.2.2 (example of a HMM), 10.3 (Inference), 10.4 (Learning - skip 10.4.1).
    * __Interesting bit__: A HMM is a more parsimonious way to represent higher-order Markov models, for example an n-order Markov chain (Section 10.2.2 right below the equation 10.9).
    * We should make a point to review Chapter 20 for inference, especially since factorization improve estimation times.


* Chapter 17
    * __Sections most related to our work__: 17.3 (HMM), 17.4. (Inference), 17.5.3 (Bayesian Learning), 17.6.2 (HHMM) and 17.6.3 (Input-Output HMM).
    * __Interesting bits__: MLE can't deal with zero-counts, which are likely when dealing with K^T free parameters (K states, m the order of the markov process), and predicts an event is impossible if not seen in the training set. Good old known arguments for bayes, but applied to our model.
    * Section 17.3 establishes the model for both discrete or continuous variables in the observation model. Was worried about this, since some papers like Hassan (2005) model continuous observations while Tayal (2009) rely on descrete feature set.
    * Section 17.4 describe inference on latent variables
        * Describes the _occasionally dishonest casino_ example. It's a very nice intuitive way to explain a HMM, and SSM in general. Not useful for our research, but useful for teaching or asking for funds to business people.
        * Types of inference: Filtering, smoothing, Fixed lag smoothing, prediction, Viterbi decoding (MAP), Posterior samples and Probability of the evidence.
        * Algorithms: Forward (filtering), forwards-backwards (smoothing), Viterbi (most probable sequence), backwards sampling.
        * Having a clear idea on all the different quantities that can be estimated is __KEY__. Each replication focuses on estimating a different quantity, and confusion may lead us to spend time in implementations that are not required.
        * _Note_ It'd be useful to have a summary table: method, quantity estimated / of interest, uses information up to ..., name of the algorithm involved, even time and space complexity in O() notation if possible.
    * Section 17.5 describes learning the parameters
        * With fully observed data: a piece of cake, ha!
        * With hidden data (the states):
            * Baum-Welch algorithm Baum (1970), i.e. the EM to find the MAP.
            * The bayesian approach (this subsection is starred, we've been warned!)
                * Variational Bayes EM.
                * MCMC based on block Gibbs sampling, which I don't think we can do in Stan (no categorical parameters allowed).
                * Discriminative training, when HMM are used as the class conditional density inside a generative classifier.
          * Model selection (ex. the number of hidden states) and other stuff we won't need for our replications.
      * Section 17.6 describes variations:
          * Variable duration (semi-Markov) HMMs to model explecitely the duration at each state.
          * _Hierarchical Hidden Markov Models_, finally!
              * Cites Fine (1998) as the main original work. The paper is already in our reading list.
              * Cites Murphy and Paskin (2001) for inference. Added the paper to the reading list.
              * Interesting bit: Higher level chains evolve more slowly than lower level chains.
          * Input-output HMM to allow for inputs (AKA control signal). Hassan (2005) use OHLC prices as inputs IIRC, and the parametrization looks very similar to equations 17.116 and 17.117.
          * Auto-regressive HMM (AKA regime switching Markov model). For continuous outputs, it's equivalent to a linear regression model, where the parameters are chosen according to the current hidden state.
          * Factorial HMM
          * Coupled HMM
          * Other complex variations that can be set up as a Dynamic Bayesian Network.

* Chapter 20 presents exact inference for graphical models. It generalizes exact inference algorithms used in previous chapters to arbitrary graphs. Describes a variety of algorithms without derivation.

* Chapters 21 and 22 present deterministic algorithms for posterior inference.

* Chapter 24 is MCMC.
    * Section 24.5.1 mentions Hamiltonian MCMC only for continuous state spaces. No mention of adaptations for discrete latent spaces like marginalization. Bummer!

* Enough reading for today, thanks for reading!

### 20170410 Mo 20 to 24 ###
* Started with Fine (1998)
    * 1. Intro
        * Pros: modeling different stochastic levels, ability to infer correlated observations over long periods in the observation sequence via the higher levels of the hierarchy.
        * Shows efficient estimation scheme inspired by the inside-outside algorithm (quadratic in the length of the observations).
        * Shows two examples of unsupervised learning.
    * 2. Model description
        * It generalize the standard HMMs by making each of the hidden states an “autonomous” probabilistic model on its own, that is, each state is an HHMM as well. The states of an HHMM emit sequences rather than a single symbol.
        * Model description is clear.
        * Notation is dense, but can be handled with enough care.
    * 3. Inference and learning
        * Three natural problems:
            * Calculating the likelihood of a sequence: a generalized version of the Baum-Welch algorithm.
                * Forwards takes O(NT^3), where N total number of states and T length of the obs sequence.
                * Backwards ?.
                * Details about calculation available in Appendix A.
            * Finding the most probable state sequence: the generalized Viterbi algorithm.
                * Takes O(NT^3), pseucode available in Appendix B.
                * An heuristic alternative that takes O(NT^2) is provided, although without theoretical justification.
            * Estimating the parameters of a model
                * MLE estimation based on generalized Baum-Welch algorithm, adding downward and upward transitions. Basically, an EM algo.
        * Solutions are more involved than for HMMs, due to the hierarchical structure and multi-scale properties. No kiddin!
        * __Estimation is difficult, we'll need to put a good deal of time and effort into this part__. Possibly one of the most challenging parts of the project.
    * 4. Applications
        * A multi-level structure for English text
        * Unsupervised learning of cursive handwriting
        * In each applications, the hidden states have very intuitive and iterpretable meanings even in the case of unsupervised learning.
    * 5. Conclusions
        * HHMMs are able to correlate structures occurring relatively far apart in observation sequences, while maintaining the simplicity and computational tractability of simple Markov processes.
        * They are able to handle statistical inhomogeneities.
        * MLE for the parameters and Viterbi encoding generalize naturally.
        * Future generalization include input-output and factorial HMMs.
    * Appendix A: detailed definiton and estimation for the quantities in the generalized Baum-Welch algorithm.
    * Appendix B: Initialization and recursion for both production and internal states.

* Added Bengio & Frasconi (1995) _An input-output HMM architecture_ to the reading list. It's the main citation by Fine (1998) for input-output HMM, which will be needed to replicate Hassan (2005).

* __Fun fact!__ Baum (1970) cites a paper called "Probabilistic models for stock market behavior" by Baum, Gaines, Petrie and Simons. It's marked as "to appear" but can't find it on Google, looks like they never published it.

* efm said Larry Williams mentioned something about using a variant of HMM for trading. Should investigate a bit and see if we can cite a practitioner.

* Started with Bengio & Frasconi (1995)
    * 1. Intro
        * Proposes to propagate, backward in time, targets (output) in a discrete state space rather than differential error information.
        * Uses EM.
        * It can be used tolearn to map input sequences to output sequences, while HMM learns the output sequence distribution.
    * 2. The proposed architecture
        * A discrete state dynamical system where a) states depend on previous state and current input, and b) the output is a function on both current state and input.
        * Admissible state transitions are specified by a directed graph.
        * There is a set of state networks and output networks, each of them is uniquely associated to one state. All the networks share the input.
            * State networks predict next state distribution given previous state and current input.
            * Output networks predict the output given current state and input.
            * Each state has a set of parameters: connection weights.
            * The internal state is computed as a linear combination of the output of the state network gated by previously computed internal state.
            * Output networks are linearly combined with the internal state to predict global output.
            * Probabilistic interpretation
                * Connection weights: probability of being in a state given the input sequence.
                * The output of the state networks are transition probabilities conditioned on the input sequence.
                * The global output is the expected output conditional on an input and a state.
                * Output density should be decided: multinomial for sequence classification (applies softmax to the output of output subnetworks), gaussian for continuous output (applies linear combination to output of the output subnetworks).
    * 3. Supervised learning
        * Training data: pairs of input/output sequences.
        * Parameters: parameters of the state and output networks.
        * EM algo: the states aren't observed.
    * 4. Comparisons
        * Computing style
            * With IOHMM, input sequence can be synchronously transformed into an output sequence.
            * Transition probabilities are conditional on the input. They depend on time and thus result in an inhomogeneous Markov chain. System dynamics aren't fixed but adapt in time to the input sequence.
        * Learning:
            * The output is used as desired output to the input, resulting in discriminant supervised learning.
    * 5. Regular Grammar Inference
        * Some classification model for text.
    * 6. Conclusions
        * Effectiveness of the model in large state spaces needs to be evaluated.
        * Since transitions probabilities are adaptative, IOHMM should deal better with long-term dependencies.
        * Sequence production or prediction capabilities should be investigated.

* Bengio & Frasconi (1995) will be a useful resource to replicate Hassan (2005). It'll be key to have Hassan's model correctly specified from day one (the number of states, the densities and the emissions) before writing any code. Thanks Captain Obvious! At first sight, the algos for IOHMM look only slighly more complex, meshing inputs in intermediate quantities (ie forward probs, backward probs, loglik). Most of Stan code I've seen for HMM don't allow for inputs, so this will take both a bit of time and creativity. Good thing is we can later contribute our variation to the Stan manual.

OK NOW I'M GETTING VERY ANXIOUS TO GIVE HASSAN MY FIRST TRY, but that won't happen until next month. Bummer!

### 20170411 Th 21 to 24 ###
* I'll start writing soon cause reading-only is boring.

* What should I include in the lit review? This is my proposed structure:
    * Theory review (2 pages for HMM, 2 pages for HHMM): 
        * Model specification (assumptions, equations, parameters, notation, etc)
        * Estimation
        * Inference algos
        * Proofs and derivation are referenced
    * Applications (4 paragraphs for description and 2 paragraphs for findings):
        * Description of the experiment (goal, methodology, data, benchmarks)
        * Results/findings (results, comparisons, unanswered questions)
        * Organized by topic (ex. models for trading, currencues, prices, market states detection, etc):
        * Maybe some overall ideas relating all the models inside a topic
        * Paper relevance and relation to the current research project.

* Started with Murphy (2001)
    * Want to see if we can profit from expressing the HHMM as a DBN to increase estimation speed.
    * 1. Intro
        * Inference for a HHMM takes O(T^3), where T is observed sequence length. Expressing it as DBN allows inference in linear time O(T).
    * 2. HHMM
        * The hidden states can emit a single observation (production state) or a sequence (abstract states).
        * The abstract strates are governed by sub-HHMM, which can be called recursively.
        * When a sub-HHMM is finished, control is returned to wherever it was called from.
        * Any HHMM can be converted into a HMM by creating a state for every possible state in the configuration.
            * If the HHMM transition diagram is a tree, there will be one HMM state for every HHMM production state.
            * If the HHMM transition diagram has shared substructure, this structure must be duplicated in the HMM.
            * The ability to reuse these sub-models in different contexts makes HHMM more powerful. Parameters only need to be estimated once.
    * 3. Cubic time inference O(T^3)
          * Inference originally presented by Fine (1998) is based on the Inside-Outside algorithm and takes O(N T^3), where N is numeber of states. This algo is explained in the paper.
    * 4. Representing the HHMM as a DBN
          * Reparametrization is explained, including topology and conditional probability distributions.
          * Reparametrization is not trivial, it includes the specification of a few transition matrixes and dummies.
    * 5. Linear time inference
          * Hope we don't have to deal with his cause it's awefully hairy. I'm serious.

### 20170412 W 20 to 23 ###
* Looking for new papers on (H)HMM applied to finance
    * Hidden markov model stocks
        * Ryden (1998) Stylized facts of daily return series and the hidden Markov model
            * Bulla (2006) Stylized facts of financial time series and hidden semi-Markov models
        * Grupta (2012) Stock market prediction using hidden Markov models
        * Bhar (2006) Hidden Markov models: applications to financial economics
        * Zhang (2004) Prediction of Financial Time Series with Hidden Markov Models
        * Dias (2010) Mixture Hidden Markov Models in Finance Research
        * De Angelis (2013) A dynamic analysis of stock markets using a hidden Markov model
        * Maheu (2012) Identifying Bull and Bear Markets in Stock Returns
        * Bulla (2011) Hidden Markov models with t components. Increased persistence and other aspects
        * Park (2009) Forecasting Change Directions for Financial Time Series Using Hidden Markov Model
        * Elliot (2001) Portfolio optimization, hidden markov models, and technical analysis of p&f-charts
    * Hidden markov model finance
        * Landen (2000) Bond pricing in a hidden Markov model of the short rate
        * Zucchini (2016) Hidden Markov Models for Time Series
        * Landen (2000) Bond pricing in a hidden Markov model of the short rate
        * Mamon (2007) Hidden Markov Models in Finance
        * Mamon (2014) Hidden Markov Models in Finance Vol II
        * Shi (1997) Taking Time Seriously Hidden Markov Experts Applied to Financial Engineering
        * Giampieri (2005) A Hidden Markov Model of Default Interaction
    * Hidden markov model trading: empty, ha
    
* At quick glance, everything is just a re-expression of regime switching models. Little related to trading.

### 20170413 Th 12 to 15 ###
* Reading Rabiner (1986)

### 20170417 Mo 20 to 24 ###
* Finally started writing the ALR on HMM.

[Missing entries]

### 20170422 Sa ###
* Spent the day cleaning and arranging the new rented flat, I'm moving!!!

### 20170423 Su 14 to 02 ###
* Finished the MC part.
* Continued with the HMM section.
* Should I use "interval", instead of time, as in "x_t is the value of x at interval t"?

### 20170425 Tu 00 to 02 ###
* Continued with the inference section for HMM, checking against Murphy.
* Next step: complete with other papers or books.
* Add cites (ex. citing the forward-backwards original paper).

### 20170425 Tu 21 to 24 ###
* Finished the inference section for HMM, checking against Murphy (again).
* Added the parameter estimation section.
* Write the algo box for other inference algos (for example, smoothing). It'd be an original work since this is not in Murphy :D.

### 20170426 We 20 to 24 ###
* Improved the HMM section with parts from Bishop (2006)
* Added many reference to the original works

### 20170427 Tu 20 to 24 ###
* Improved the HMM section with parts from Rabiner (1989)
* Added many reference to the original works
* The section is finally taking shape, yay!
* Log: Algorithm 17.1 in Murphy (2012) - alpha_1 in both lines 2 and 4 should be bold (it's a vector).

### 20170507 Su 13 to 21 ###
* At this point, you musta realised the whole logging thing got me bored
* Went on writing the IOHMM part. Notation unification is a real pain, taking more than I actually thought.

### 20170508 Mo 21 to 01+1 ###
* Bonding time! A kick-off email to the mentors (means of contact, time schedules, recap, planification).

### 20170509 Tu 21 to 01+1 ###
* Added CC-BY-SA 4.0 License.
* Fixed a few problems with bibliography and formats.
* Finally pushing to GitHub: https://github.com/luisdamiano/gsoc17-hhmm

---

# Stage II: Hassan 2005 #

### 20170522 Mo 13 to 22 ###
* Starting coding for a plain vanilla HMM in R (simulation) and Stan (estimation). Goal: instead of shooting for IOHMM as first implementation, I broke it down into two steps a) HMM, and b) then I add the needed modifications.
* Update: flight delayed for about two/three hours, extra coding time woohoo!
* Update 2: Coding in the sky is cool!
* Ok landing. I think the FF in matrix form works, which is a good thing since it's pretty speedy. Still have to see if I can avoid refactoring the transition matrix. Was only able to check it with a small example, should try with something larger but it's landing time.

### 20170523 Tu 21 to 23 ###
* Testing my hmm code.
* Should check the likelihood specification of the model.
* Should write the Forward algo in terms of log_sum_exp for better numerical stability.

### 20170524 We 15 to 18 ###
* Found a few typos, can't recover the parameters from the simulation yet.
* Working on log scale made this super slow.

### 20170525 Th 20 to 01+1 ###
* hmm.stan has divergences in some chains. In those mixing well, forward filter and viterbi can recover the parameters from the simulations YAY! Still very unstable (I get convergence in approx 1 out of 4 chains - others get stuck)
* The matrix implementation for the forward filter doesn't work. I'll leave this to focus on hmm.stan.

### 20170526 Fr 21 to 03+1 ###
* The maximization problem is very sensitive to initial values, especially since chains may get stuck in local maxima. Initializing the observation means with the estimates from k-means makes the hmm estimation more efficient.

### 20170527 Sa 04 to 07 ###
* Can't sleep, but can (break) code tho! Made coffee to help me write two types of inferences still missing in my code: smoothing and prediction.
* Just learnt the hard way that Stan doesn't allow for decreasing loops, say T:1. The trick is to setup t = 1:T and then use T-t as a decreasing index.

### 20170527 Sa 18 to 19 ###
* Had to downgrade to RStan 2.14 since v2.15 introduced a bug in print() that wouldn't let me debug my code.

### 20170527 Sa 21 to 02+1 ###
* The smoothed probs look funny, I'm reviewing my code equation by equation.
* Ok, finally got my code working.
    * Stan implementation currently does:
        * Estimate the parameters (initial prob, transition prob, mu/sigma for each state)
        * Make inference on filtered and smoothed probs
    * R script currently does:
        * Simulate HMM from given parameters values
        * Print summary and plot dynamics of current Stan implementation
    * Future steps:
        * Trim/clean code
        * Optimize speed (prefer vectorization, there are some redundant loops)
        * Document
        * Move into IOHMM

### 20170528 Su 22 to 01+1 ###
* Run a few more tests on the hmm-ex code.
* Went on with the lit review for the IOHMM, we'll start writing Stan code soon.

### 20170529 Mo 19 to 20 ###
* Run some extra tests on the hmm-ex code, namely linear normalization versus softmax on the forward probability (alpha).

### 20170529 Mo 22 to 02+1 ###
* Run some extra tests on the hmm-ex code, namely linear normalization versus softmax on the forward probability (alpha).
* Went on with lit review about IOHMM, especially the filtering and smoothing problems.

### 20170530 Tu 15 to 19 ###
* Went on with lit review about IOHMM. Got all the equations summarized in my paper notebook, haven't typesetted them yet.
* Started working in iohmm_sim, an R function to generate data from a IOHMM model given the set of parameters. Implementing the data generation steps helped me grasp the details about how the model works.

### 20170530 Tu 22 to 05+1 ###
* Adapted the hmm.stan code for iohmm. It compiles. Little divergences, horrible fit. Possible problems: underidentification.
* For a first trial, I'm running the sampler with the real parameters as initial values to assess if the stan code works as intended.

### 20170531 We 08 to 10 ###
* Went on with lit review about IOHMM, typesetting.

### 20170601 Th 21 to 23 ###
* Went on with lit review about IOHMM: re-read the whole document looking out for typos, format and notation issues.

### 20170602 Fr 23 to 02+1 ###
* Went on with lit review about IOHMM: re-read the whole document looking out for typos, format and notation issues.

### 20170603 Sa 11 to 19 ###
* Went on with lit review about IOHMM: re-read the whole document looking out for typos, euqation references, format and notation issues.
* Sent check-in email.

### 20170604 Su 15 to 19 ###
* Went on with lit review about IOHMM: re-read the whole document looking out for typos, euqation references, format and notation issues.
* Went on with iohmm.stan

### 20170604 Su 22 to 23 ###
* Went on with iohmm.stan

### 20170605 Mo 20 to 02+1 ###
* Went on with iohmm.stan. After many tries, I learnt that the sampler suffer from divergences when computing the smoothed prob in this way:

```stan
    for(t in 1:T)
      gamma_tk[t] = normalize(ungamma_tk[t]);
```

alpha_tk and beta_tk are both in log scale, but it's now clear to me that there must be an underflow somewhere. I would have never realised if it weren't for the divergences, so I'm now totally convinced that divergences are a *feature* of HMC.

* Started working on the delivery.

### 20170606 Tu 20 to 01+1 ###
* Remember the divergences from yesterday?

```stan
    for(t in 1:T)
      gamma_tk[t] = normalize(ungamma_tk[t]);
```

The normalization routine would divide-underflow it seems:

```stan
functions {
  vector normalize(vector x) {
    return x / sum(x);
  }
}
```

So I just implemented this quick fix, which is an ugly hack but it works!

```stan
functions {
  vector normalize(vector x) {
    real denom = max([sum(x), 0.00000001]);
    return x / denom;
  }
}
```

I'll have to see to a better implementation, of course.

### 20170608 Th 20 to 22 ###
* Adding observation log and other adhoc I'll need for paper replication.

### 20170609 Fr 21 to 22 ###
* Computed observation loglik, as will be required by the paper.

### 20170610 Sa 11 to 19 ###
* Fixed iohmm-ex.stan code.
* Run extensive controls on the Stan results.
* Worked on exploratory and diagnostic plots and their documentation.
* The code is almost ready to plug in real data, very soon!

### 20170611 Su 21 to 23 ###
* Worked on exploratory and diagnostic plots and their documentation.
* Check-in email.

### 20170612 Mo 20 to 02+01 ###
* Created a new plot fit fitted vs actual.
* Started writing our replication paper.

### 20170613 Tu 20 to 22 ###
* I observed that hard classification based on filtered probability is very precise as expected, but smoothed probability isn't. Also, their relationship looks weird. Shall check the code for the forwards-backwards algo.
* Nevermind, a problem with loop indices, it works now.

### 20170614 We 20 to 22 ###
* Worked on the vignette.

### 20170615 Th 22 to 02+1 ###
* Pushed a cache folder last night. Fixed my gitignore.
* Folder structure needed a few changes.
* Worked on iohmm-mixture, which is the stan code that will be used for the replication (an IOHMM where the observation model is a mixture of gaussians).
* I should rework the README file a bit too.

### 20170617 Sa 10 to 20 ###
* Went on with the mixtures in iohmm-mixture. Sampler works for L = 1 component, but it hits maxtreedepth for L = 3 :( Having trouble dealign with mixtures inside hidden states based on regressión parameters, too many hidden things I guess. Will have to spend more time doing diagnostics.
* Improved plots
* Improved the section of the README file.
* Chains still look funky, but I'll have to see to this tomorrow. Some look non stationary and are highly autocorrelated. Estimated parameters aren't well off. Things to try tomorrow: 1) more data, 2) better separated data, 3) better initial possition, 4) larger warmup??.

### 20170618 Su 10 to 22 ###
* Will try many of the alternatives stated above and see how the sampler reacts. Need to make sure the sampler recovers the parameters before plugging in real data, which won't be as well behaved as simulations.
* Increased distance among clusters, larger dataset, good initial values and larger sampling size and warmup improved sampling performance largely. Convergence and speed are now remarkably good. All parameters were well recovered, although sampling efficiency is low for the parameters of the hidden-state multinomial regression (number of effective sampling size drops from 300 to 30).

```R
# Data
T.length = 300
K = 2
L = 3
M = 4
R = 1
u.intercept = FALSE
w = matrix(
  c(1.2, 0.5, 0.3, 0.1, 0.5, 1.2, 0.3, 0.1, 0.5, 0.1, 1.2, 0.1),
  nrow = K, ncol = M, byrow = TRUE)
lambda = matrix(
  1/3,
  nrow = K, ncol = L, byrow = TRUE)
mu = matrix(
  1:(K*L),
  nrow = K, ncol = L, byrow = TRUE)
s = matrix(
  0.1,
  nrow = K, ncol = L, byrow = TRUE)
p1 = c(0.5, 0.5)

# Markov Chain Monte Carlo
n.iter = 600
n.warmup = 300
n.chains = 1
n.cores = 1
n.thin = 1
n.seed = 9000

# ...

#                   mean se_mean      sd     50% n_eff Rhat
# p_1k[1]         0.3382 1.4e-02 0.23470  0.3126   300  1.0
# p_1k[2]         0.6618 1.4e-02 0.23470  0.6874   300  1.0
# w_km[1,1]       1.2615 6.4e-01 3.74994  1.2759    34  1.1
# w_km[1,2]      -0.0236 2.5e-01 3.80345  0.1021   233  1.0
# w_km[1,3]      -0.6970 5.2e-01 2.96839 -0.9629    33  1.0
# w_km[1,4]      -0.8778 4.6e-01 3.22484 -0.7979    49  1.1
# w_km[2,1]       1.3951 6.4e-01 3.74191  1.3767    34  1.1
# w_km[2,2]       0.0841 2.5e-01 3.81057  0.1201   235  1.0
# w_km[2,3]      -0.7028 5.2e-01 2.96406 -0.9990    33  1.0
# w_km[2,4]      -1.0408 4.6e-01 3.23715 -0.8777    49  1.1
# lambda_kl[1,1]  0.3377 2.0e-03 0.03388  0.3338   300  1.0
# lambda_kl[1,2]  0.3296 2.0e-03 0.03536  0.3292   300  1.0
# lambda_kl[1,3]  0.3327 2.0e-03 0.03461  0.3328   300  1.0
# lambda_kl[2,1]  0.3792 2.2e-03 0.03796  0.3796   300  1.0
# lambda_kl[2,2]  0.3197 2.1e-03 0.03570  0.3198   300  1.0
# lambda_kl[2,3]  0.3011 2.1e-03 0.03585  0.2991   300  1.0
# mu_kl[1,1]      1.0008 7.6e-05 0.00132  1.0007   300  1.0
# mu_kl[1,2]      1.9985 9.0e-05 0.00149  1.9985   274  1.0
# mu_kl[1,3]      2.9971 9.2e-05 0.00150  2.9971   267  1.0
# mu_kl[2,1]      4.0007 1.1e-04 0.00148  4.0006   198  1.0
# mu_kl[2,2]      5.0000 9.3e-05 0.00161  5.0001   300  1.0
# mu_kl[2,3]      6.0011 9.7e-05 0.00168  6.0011   300  1.0
# s_kl[1,1]       0.0088 4.9e-05 0.00085  0.0087   300  1.0
# s_kl[1,2]       0.0116 6.3e-05 0.00109  0.0115   300  1.0
# s_kl[1,3]       0.0107 6.9e-05 0.00119  0.0106   300  1.0
# s_kl[2,1]       0.0110 7.2e-05 0.00125  0.0109   300  1.0
# s_kl[2,2]       0.0103 7.3e-05 0.00113  0.0103   238  1.0
# s_kl[2,3]       0.0108 7.6e-05 0.00131  0.0107   300  1.0
```

* I tried many variations to see the sensibility of the results to many of the simplifications stated above. I removed the features one by one (a sort of ceteris paribus or partial derivative analysis).
* Removed initial values: sampling time doubled (from 5 to 10 mins - treedepth increased from 7 to almost 10, which is the cause of the problem), all parameters were perfectly recovered, sampling efficiency remains the same. This is **not** surprising since mixture density are multimodal and any algorithm (either frequentist or bayesian) would either need good init values or several starting points.
* Decreased distance between clusters: with `s = matrix(1, K, L)`, clusters overlap largely. Initial values given by kmeans are good but noisy. Sampling time improved (from 5 to 2 mins - treedepth improved from 7 to 6, which is a good figure), parameters are recovered with a large degree of uncertainty as expected, sampling efficiency drops significantly for all the parameters (from 300 to 30-60).
* Added complexity: `K = 4`, as in Hassan (2005). Initial values given by kmeans are good but noisy. Sampling time increased a huge lot (from 5 to 30 mins, 6 times, hitting treedepth at max levels =/), parameters recovery is reasonable although sampling efficiency dropped significantly for all the parameters (from 300 to 30-60).
* After many tries, I think the model works but it's pretty data-greedy. It works very well with K = 2 hidden states, but it needs much more data to accomodate K = 4 hidden states.
* Started writing the replication section of the essay.

### 20170619 Mo 20 to 02+01 ###
* Back to work, decided to get that sampler working!
* Adding a hyperparam to mean components helped a lot: computational time reduced 70%, mixing, convergence and sampling efficiency greatly improved too.
* Made many little changes into iohmm-hmix.stan tofor small gains in efficiency.
* New changes in folder structure, again :)
* Plugged in real data and, of course, it didn't work. 200 samples = 200 divergences, HMC is hinting me that something is wrong:
  * All observations are assigned to just one state.
  * In consequence, the fitted x stays close to approx. 48.
  * Tried with two hidden states and one component (no mixture) and sampler doesn't converge with real data. It converges with simulated data of various type, including stationary io, non stationary output, but it doesn't for non stationary io.
  * Tried with iohmm-reg and I get many divergences. It recovers only two hidden states. The actual vs fit looks reasonable but a bit biased.
* Possible solutions:
  * Standarize the inputs and/or outputs? First difference?

### 20170620 Tu 10 to 22 ###
* Today I expect a marathon that should get me closer to the final version of the first paper.
* A simple model with K = 2 and L = 1: scaling (demeaning and dividing by sd) inputs fixed most of the divergence issues. Scaling both input and output fixed all but one divergent iteration. Samplig efficiency reached it's maximum.
* An intermediate model with K = 4 and L = 1 with scaling input and output: No divergences. Sampling efficiency dropped for a few parameters (mu for two states went from 200 to 20, sd for one state from 200 to 58). Computational time increased by a magnitude of 10 (treedepth from 3 to 7). Fit given the model is reasonable.
* The goal model with K = 4 and L = 3 with scaling input and output: No divergences. Sampling efficiency dropped for a few component parameters vs the simple model (mu for two components went from 200 to 20, mu for another two from 200 to 63 and 95, sd for one component from 200 to 58). Computational time increased by a magnitude of 20 vs the simple (treedepth from 3 to 8) and 2 vs the intermediate. Fit given the model is reasonable. 
* Wrote the code that fetches and handles the data.
* Wrote the code that follows the forecasting procedure in the original work.
* Created a lite version of the sampler that computes only the quantities needed to make forecats.

### 20170621 We 22 to 23 ###
* Fixed forecasting loops.
* Left the computer walk forward the series during the night.

### 20170622 Th 21 to 24 ###
* Working on the paper.

### 20170624 Sa 11 to 20 ###
* Working on the paper.

### 20170625 Su 10 to 21 ###
* Working on the paper.
* Mental note: I should take a scientific writing 101 course.

### 20170626 Mo 21 to 24 ###
* Working on the paper (forecast).

### 20170701 Sa 9 to 18 ###
* Finally back to end this!
* Worked on the forecast and discussion sessions.

### 20170702 Su 10 to 22 ###
* Re-read the whole report. Fixed many minor issues. Checked there's no old/broken code in the rmarkdown file. Addressed some writing style issues. Improved a few explanations and descriptions.
* Sent check-in email.
* To-Do:
  * Will run the whole code from the beginning to make sure everything works out of the box.
  * Fix plot legend expression
  * Fix plot legend output fit

---

# Stage III: Jangmin 2004 #

### 20170708 Sa 09 to 19 ###
* Back after a hard week, sorry
* Worked on theory (litreview)
* Started the data simulation function (still WIP)

### 20170709 Su 09 to 19 ###
* Went on with the data simulation function, learning a lot about recursion

### 20170710 Mo 19 to 23 ###
* No paper describes a HHMM with enough generality, they all miss some details. Had to read all the papers and aggregate the rules of node activation.
* Wrote the simulation function and got to test it with a simple 2-component gaussian mixture. It worked.

### 20170711 Tu 22 to 01+1 ###
* More testing for the simulation routine.
* Tested on Fine (1998) and it seems to work.
* To-do: Use the simulation routine to draw samples from Jangmin (2004)'s model.

### 20170712 We 22 to 24 ###
* Used the simulation routine to draw samples from Jangmin (2004)'s model.

### 20170715 Sa 10 to 20 ###
* Working on Jagmin replication (data adquisition and preprocessing).

### 20170716 Su 10 to 20 ###
* Working on Jagmin replication (data adquisition and preprocessing).

### 20170717 Mo 21 to 21 ###
* Started working on Stan code.
* Each node has it's own initial distribution vector and transition matrix with sizes varying along nodes. Stan doesn't allow for ragged arrays. I'll have to work with a full database (or long) format and then apply softmax to force a subset into a simplex. This may lead to some problems since posterior could easily become improper when priors are uniform (see)[https://groups.google.com/forum/#!topic/stan-users/6QLSOM3ySYA]. In consequence, I'll need to specify (some vague) prior information on the softmax parameters.

### 20170718 Tu 21 to 02+1 ###
* After staring at the appendix of Fine (1998) for hours, I *think* I may know how to implement the recursive algorithm. Will give it a go tomorrow. Will use an adjacent matrix for node identification.

### 20170720 Th 20 to 24 ###
* I'm stuck. Can't device a way to program the node dynamics without objects/structs/arrays or other non-primitives.
* Fired an SOS email to the mentors and (created a thread on Stan forums)[http://discourse.mc-stan.org/t/transversing-up-a-graph-hierarchical-hidden-markov-model/1304]. Hopefully I'll get some input that will get me on the move again.
* I (created a thread on Stan forums)[http://discourse.mc-stan.org/t/identifiability-and-convergence-input-output-hidden-markov-model/1305] about Hanssan replication, hoping to get some feedback to improve convergence.

### 20170722 Sa 10 to 17 ###
* Trying a few ideas originated in the discussion at the the Stan forums.
* I created a very simple graph model, a toy example with most of the main features of our real model. Wrote a simulation routine in R, coded the model in Stan and it seems to recover the parameters right. It's a good lead. I'll increase the complexity in view of the real one.

### 20170723 Su 9 to 21 ###
* Adding semisupervision to the toy model.
* Improved part of the code using tips provided by (Bob Carpenter)[http://discourse.mc-stan.org/t/identifiability-and-convergence-input-output-hidden-markov-model/1305/4]. Code speed improved by a factor of 3, the man is a genius!
* With calibration by simulation I could confirm that semisupervised hhmm works for a hierarchical 2x2 gaussian mixture. I'm almost there to check if it works for data simulated from the graph presented in jangmin.
* Check-in email.

### 20170726 We 22 to WIP ###
* Trying to fit my Stan code to simulated data following Jangmin graph.
* 100 obs + 200 samples (including 100 burn-in) + 23 states = 25 minutes.
* Simplification I: reduce from 63 to 23 states by treating observations in groups of three.
* Simplification II: force zeroes in production nodes that aren't connected (ex. node inside sb from node inside su).
* Check-in email.

### 20170728 Fr 16 to 17 ###
* Hangout session with Michael.
* Summary of the discussion.

### 20170729 Sa 10 to 18 ###
* Reading Tayal(2009) in great detail:
  * DBN representation
  * HHMM representation
  * Characteristics and strategies that could help improve computational times (ex. conditional independence)
  * Estimation algo
  * Rewriting the model in probabilistic terms (in the form of P(z_1 | z_2, ...) ~ )
* Should start with Gold (TSE:G). It's the stock described in most of the figures of the original work. It's also in the group with most volume and the model and the model works best for high volume trends.

### 20170730 Su 10 to 21 ###
* ...

### 20170731 Mo 19 to 22 ###
* ...

### 20170801 Tu 21 to 24 ###
* ...

### 20170802 We 21 to 01+ ###
* ...

### 20170803 Th 18 to 01+ ###
* Finally! The Stan code for the hmm representation of Tayal's hhmm works.

### 20170804 Fr 11 to 12 ###
* Short email to mentors.
* Speed tests.

### 20170805 Sa 10 to 18 ###
* Started writing the feature extraction code.
* Using some example data from highfrequency while we fetch real data.
* I'll try to write this in a way that it'll be easier to submit as a PR for highfrequency later.

### 20170806 Su 9 to 20 ###
* Continued with the feature extraction code.

### 20170808 Tu 23 to 02+1 ###
* ...

### 20170809 We 23 to 02+1 ###
* Working on the write-up about feature extraction.
* Searching entries with JabRef.
* Read related works, Wisebourt (2011) and Sandoval & Hernández (2015), looking for details behind the feature engineering process.
* Reviewed GSoC final submission guidelines.
* Sent a check-in email.
* Interaction on Stan list.

### 20170810 Th 21 to 02+1 ###
* Working on the write-up about feature extraction.

### 20170811 Fr 19 to WIP ###
* Spun up an ubuntu box (virtualbox) to run financialinstrument's parser.
* Parsed data.
* First inspection says data will be of much help.

## Final notes

Log keeping became more and more difficult when the deadline become closer. Sorry for not being able to keeping this up. You do know the ending tho (spoiler alert!): finally got the project done! Thanks for reading!

---

# Notes

## Notation disambiguation

+---------------+---------------+--------------------+
| Paper         | This work     | Bengio             |
+===============+===============+====================+
| Input         | $u_t$         | $u_t$              |
|               |               |                    |
+---------------+---------------+--------------------+
| Output or     | $x_t$         | $y_t$              |
| emission      |               |                    |
+---------------+---------------+--------------------+
| Latent states | $z_t$         | $x_t$              |
|               |               |                    |
+---------------+---------------+--------------------+
| Num of states | $K$           | $n$                |
|               |               |                    |
+---------------+---------------+--------------------+
| Transition p. | $\Psi(j, i)$  | $\phi_{ij, t}$     |
| from j to i   |               |                    |
+---------------+---------------+--------------------+
| State prob    | $\alpha_t(j)$ | $\zeta_{j,t}$      |
| at t state j  |               |                    |
+---------------+---------------+--------------------+
| Prediction at | $x_t | z_t =j$| $\eta_{jt}$        |
| at t state j  |               |                    |
+---------------+---------------+--------------------+
| Output density| $\psi_t(j)$   | f_{\mat{x}}(\mat{x}_t, \mat{eta}_t)$  |
| or evidence   |               |                    |
+---------------+---------------+--------------------+
