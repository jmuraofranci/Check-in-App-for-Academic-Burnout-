# AdaptiveAI-CAT
Adaptive Check-in App for Academic Burnout in Graduate Students at Georgetown University
 
**In collaboration with :**  
- Andrés Jaimes  
- Akmaral Toktonazarova  

---

## Project Overview

**HoyaMind** is a speculative mobile app prototype designed to assess academic burnout among graduate students at Georgetown University. Built using adaptive measurement principles and AI-generated test items, the app offers a quick, personalized check-in experience and connects students with wellness resources based on their estimated burnout level.

---

## Key Features

- **Computerized Adaptive Testing (CAT):**  
  Efficiently estimates burnout level with 5–10 true/false items using IRT-based adaptive testing logic.

- **AI-Based Item Generation:**  
  Items are generated using OpenAI’s GPT-4 (April 2025 version) with prompt engineering strategies to ensure relevance and difficulty calibration.

- **Personalized Recommendations:**  
  After the test, students receive feedback and campus wellness resources tailored to their burnout tier (low, moderate, or high).

- **Behavioral Nudges:**  
  Supportive, stigma-reducing language is used to encourage engagement with mental health resources.

- **Minimalist UI:**  
  Developed in R Shiny with Georgetown's official blue/gray color palette.

---

## Methodology

### Item Generation
- Generated using ChatGPT with refined prompt engineering.
- Items controlled for difficulty and aligned with burnout symptoms.

### Reliability Testing
- Cronbach’s Alpha = **0.94** (high internal consistency).
- Reliability analysis done in R.

### Item Calibration
- 2-Parameter Logistic (2PL) IRT model used.
- `mirt` R package applied to estimate item discrimination (a) and difficulty (b).
- Discrimination values: **1.9–3.3**
- Difficulty values: **-0.9–1.1**

### CAT Algorithm
- First item targeted at θ = 0.
- Next item selected to maximize information based on updated θ estimate.
- Uses Maximum Likelihood Estimation (MLE) for ability estimation.

### Stopping Rules
- Minimum: 5 questions  
- Maximum: 10 questions  
- Stop early if SE ≤ 0.1  
- Stop if θ is > 1 SD from mean

---

## Burnout Levels

| Burnout Tier | Theta Range      | Example Resources                                                  |
|--------------|------------------|---------------------------------------------------------------------|
| Low          | θ < -1           | HOYA Wellness Wheel, LRED, Self-Care Tools                         |
| Moderate     | -1 ≤ θ ≤ +1      | Group Fitness, CAPS, HoyaWell, SOS                                 |
| High         | θ > +1           | MLOA, Mental Health Fund, Mental Health Screening Tools            |

---

## UI and Visualizations

- **Item Characteristic Curve (ICC)**  
- **Item Information Curve (IIC)**  
- **Standard Error (SE)**  
- **Estimated Burnout Level (θ)**  
All visualizations rendered dynamically as part of the CAT workflow.

---

## Limitations

- **Not Clinically Validated:** Items should be co-developed with psychologists for real deployment.
- **Simulated Data:** CAT algorithm and IRT model tested using synthetic student data.
- **Short Form Test:** 5–10 items may not capture full burnout spectrum but is intended as a low-barrier check-in tool.

---

## Future Work

- Pilot with real users and collect empirical response data.
- Integrate natural language input with adaptive item branching.
- Expand item pool to include multidimensional burnout traits (e.g., emotional, physical).

---

## References

Key references include:
- OpenAI (2025), ChatGPT LLM
- Chan et al. (2025), *Computers and Education: AI*
- Hambleton et al. (1991), *Fundamentals of Item Response Theory*
- Paek et al. (2023), *Educational and Psychological Measurement*
- UCLA Institute for Digital Research (n.d.), Cronbach's Alpha
- SenthilKumar et al. (2023), *AJP Heart and Circulatory Physiology*


