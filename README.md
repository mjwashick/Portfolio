# Marshall Washick — Analytics Portfolio

**Quality improvement and clinical analytics in prehospital and emergency care.**
M.S., NRP · Continuous Quality Improvement Manager & Agency Data Officer, D.C. Fire and EMS · Co-Chair, Measure Analysis and Research Committee, National EMS Quality Alliance

This repository holds selected analyses, code, and improvement work from my career in EMS and public health analytics. Everything here was produced for an operational decision — a protocol change, a deployment choice, a capital investment, a hospital negotiation — not as a coursework exercise.

**No patient data is committed to this repository.** Analysis scripts read from local, uncommitted data directories. All published figures are aggregate; hospitals appear under coded identifiers except where already public.

---

## Start here

| Analysis | Question | Methods |
|---|---|---|
| [Social Vulnerability and Hospital Destination](analyses/svi-hospital-destination.html) | Does neighborhood social vulnerability predict which hospital an ambulance takes you to? | Multinomial logistic regression, interaction terms, census-tract SVI linkage |
| [Hospital Liaison Officer Program](analyses/hospital-liaison-officer.pdf) | Did placing staff in emergency departments reduce ambulance drop times, and what did it buy the system? | Shewhart control charts (X̄, S, Individuals), variance-stabilizing transformation, log-linear regression |
| [Video Laryngoscopy Systems](analyses/video-laryngoscopy-oat.pdf) | Which airway equipment and crew configuration produces faster intubation? | Design of experiments, orthogonal array L12(3¹,2⁴), run charts, small multiples |
| [ePCR Weight Documentation](figures/qi-weight-documentation/) | Can documentation compliance be moved without adding burden? | Run charts, shifted medians, sequential improvement cycles |

---

## Analyses

### Social Vulnerability as a Predictor of Hospital Destination
*April 2025 · [Rendered report](analyses/svi-hospital-destination.html) · [PDF](analyses/svi-hospital-destination.pdf) · [Source](code/svi_analysis.qmd)*

D.C. Fire and EMS is the sole 911 transport provider in the District, which raises a question with real equity stakes: are patients from more socially vulnerable neighborhoods systematically routed to different hospitals than patients with the same clinical need from elsewhere?

I fit a multinomial logistic regression on 10,659 adult transports, modeling hospital destination against CDC/ATSDR Social Vulnerability Index percentile rank joined at the census-tract level, controlling for age, sex, race and ethnicity, and testing EMS unit type (ALS vs. BLS) as a moderator of the SVI effect. Three receiving hospitals offering equivalent service lines — trauma, STEMI/ACS, and stroke — were compared, so destination differences are harder to explain away by clinical necessity.

The model found that patients from higher-SVI census tracts were significantly less likely to be transported to one academic medical center relative to the reference hospital, that the effect was not uniform across receiving facilities, and that ALS units sorted strongly by acuity across both comparisons.

### The Impact of a Hospital Liaison Officer Pilot on ED Turnaround Time
*[PDF](analyses/hospital-liaison-officer.pdf)*

Ambulance crews in the District routinely held patients on stretchers for hours waiting for an ED bed. The department piloted a Hospital Liaison Officer — an officer plus two EMTs stationed inside the ED to observe low-acuity patients and route appropriate patients directly to triage — at Howard University Hospital in April 2023, then expanded to United Medical Center.

I evaluated it with three families of Shewhart charts, applying a square-root transformation to time-in-seconds to stabilize variance and preserve normality assumptions, with subgroup analysis by agency and by the four hospitals receiving over 70% of transports. A log-linear regression then translated the drop-time change into what leadership actually needed to know: recovered system capacity, expressed in additional 911 responses available for the same committed unit-hours.

### Analysis of Video Laryngoscopy Systems Using Orthogonal Array Testing
*January 2025 · [PDF](analyses/video-laryngoscopy-oat.pdf)*

Part of a year-long national airway management collaborative (EQuIP). Rather than test one variable at a time, I used a design-of-experiments approach — an L12(3¹,2⁴) orthogonal array — to evaluate three video laryngoscope systems simultaneously against four other hypothesized factors: one- vs. two-person technique, bougie vs. rigid stylet, BVM size, and ventilation feedback. Five paramedics each completed 12 randomized intubations on a high-fidelity simulator, with intubation intervals measured instrumentally rather than by stopwatch.

Twelve runs instead of the 96 a full factorial would have required. Hypotheses were stated in advance, including predictions that no difference would be found between systems.

### ePCR Weight Documentation Improvement
*2021 · [Charts](figures/qi-weight-documentation/)*

A conventional QI series worth reading in order: baseline run chart, three sequential improvement experiments, and goal achievement. Weekly documentation compliance moved from a baseline median near 29% to sustained performance near 99% across 2021, with the median re-centered at each demonstrated shift rather than smoothed across the whole period.

---

## Code

**[`code/svi_analysis.qmd`](code/svi_analysis.qmd)** — Quarto source for the SVI analysis. Includes custom helper functions to extract odds ratios, confidence intervals, and multiplicity-corrected p-values from `nnet::multinom` objects and inject them into `gtsummary` tables, which do not support multinomial models natively. Tidyverse throughout; tables via `gt`/`gtsummary`; effects via `ggeffects`.

---

## Figures

Selected visualization work in [`figures/`](figures/) — annotated run charts, cumulative sum charts for cardiac arrest, a t-chart for time between rare events, ridgeline and carpet plots of CPR pause distributions from defibrillator device data, and before-and-after stroke scale distributions.

## Presentations

[`presentations/`](presentations/) — including *The State of Cardiac Arrest Performance in the District of Columbia*, presented to the D.C. Resuscitation Collaborative in July 2021, covering system performance from 2016 through 2021.

---

## Methods index

`Multinomial logistic regression` · `Generalized linear models` · `Statistical process control (Shewhart X̄, S, Individuals, t-charts)` · `Run chart rules and shifted medians` · `Design of experiments / orthogonal arrays` · `Log-linear regression` · `Variance-stabilizing transformation` · `Geospatial linkage at census-tract level` · `Small multiples and distributional visualization`

`R` · `Quarto` · `RMarkdown` · `SQL` · `tidyverse` · `nnet` · `gt` / `gtsummary` · `ggeffects` · `patchwork`

---

## Contact

marshall.washick@gmail.com
