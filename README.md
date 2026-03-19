# Home Advantage in Euroleague Basketball (2010–2024)

Στατιστική ανάλυση του πλεονεκτήματος έδρας στη Euroleague, βασισμένη σε 3.444 αγώνες από τις σεζόν 2010–2024 (εξαιρουμένων των COVID σεζόν 2019–20, 2020–21).

## Βασικά Ευρήματα

- Οι γηπεδούχοι κερδίζουν στο **62%** των αγώνων (Χ² test, p < 2.2×10⁻¹⁶)
- Το πλεονέκτημα είναι **σταθερό** σε group stage και knockout (p = 0.70)
- Οι ισχυρές ομάδες εντός έδρας vs αδύναμες: **92.4%** ποσοστό νίκης
- Η έδρα ενισχύει τον PIR κατά ~11 μονάδες

## Μεθοδολογία

| Βήμα | Μέθοδος |
|------|---------|
| Συνολικός έλεγχος | Χ² test ανεξαρτησίας |
| Σύγκριση δεικτών | Paired t-test / Wilcoxon signed-rank |
| Κανονικότητα | Shapiro-Wilk |
| Clustering ομάδων | K-Means (k=3) σε PCA scores βάσει PIR |
| Αξιολόγηση clustering | Silhouette score (μ.ο. 0.62) |
| Match-up ανάλυση | Kruskal-Wallis + Dunn post-hoc |

## Δομή Αρχείων

```
├── euroleague_home_advantage.R   # Κύριος κώδικας ανάλυσης
├── report/
│   └── home_advantage_report.pdf # Αναλυτική αναφορά
└── README.md
```

## Δεδομένα

Τα δεδομένα αντλούνται αυτόματα μέσω του R πακέτου [`euroleagueR`](https://github.com/JaseZiv/euroleagueR).

## Εκτέλεση

```r
# Εγκατάσταση euroleagueR (μία φορά)
remotes::install_github("JaseZiv/euroleagueR")

# Εκτέλεση ανάλυσης
source("euroleague_home_advantage.R")
```

## Εργαλεία

R · tidyverse · euroleagueR · cluster · rstatix · ggplot2
