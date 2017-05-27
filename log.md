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

### 20170526 Fr 21 to 02+1 ###
* The maximization problem is very sensitive to initial values, especially since chains may get stuck in local maxima. Initializing the observation means with the estimates from k-means makes the hmm estimation more efficient.

---

# Notes

## Notation disambiguation

+---------------+---------------+--------------------+
| Paper         | This work     | Bengio             |
+===============+===============+====================+
| Output or     | $x_t$         | $y_t$              |
| emission      |               |                    |
+---------------+---------------+--------------------+
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
| Prediction at | $x_t | z_t =j$| $\eta_{jt}         |
| at t state j  |               |                    |
+---------------+---------------+--------------------+
| State prob    | $\alpha_t(j)$ | $\zeta_{j,t}       |
| at t state j  |               |                    |
+---------------+---------------+--------------------+

## Questions - Stuff to discuss with Brian and Michael

* Statistical vs ML jargon: should be pick one and stick to it, or simply use the interchangeably? for example, learning vs estimation.
* Citing policy: should be cite papers we don't read, but are cited in the work we did read, in order to leave a reference is the reader is interested? Consider this practical example:
    * We read Bengio & Frasconi (1995) on IOHMM. I'm stating other related or equivalent models, and he mentions "This model can also be seen as a recurrent version of the Mixture of Experts architecture (Jacobs et al., 1991)". Should we only mention the equivalence to Mixture of Expects or should be also cite Jacobs in case somebody is interested in this model, even if we've not read the paper?
* Referring to $t$: the subindex can represent time, space, or even any other sequencial index. Is there any generic word that would comprise both time and space? I'd used *point* (and sometimes step) so far. Alternatively, I could state at the beginning of the work that we'll use time but this works with space too?

## TODO

* When using equations inside a table, find a better way to escape the pipes.


