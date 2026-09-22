# Anomaly detection on machine sensor data

Detecting failures in a mechanical processing line from the electricity the machine
draws. Written in March 2023, during my MA at Bergamo.

## The problem

A machine on an assembly line processes pieces one after another. A sensor on it records
electric consumption at 40 Hz. When the machine starts to wear or malfunction, that
shows up in the trace — but the series is irregular enough that you cannot see it by
eye, and the point of detecting it at all is to intervene straight away, so the
detection has to work on data as it arrives rather than after the fact.

Anomalies vary in both length and shape. Each row carries a label marking the times the
machine failed, and there is a separate training and test set. The data belongs to the
client and is not in this repository.

## Approach

I fitted logistic regression, random forest, isolation forest and XGBoost. Random forest
and XGBoost came out best, so I stacked them: their posterior probabilities go into a
final logistic regression, and the positive class is called at a threshold of 0.4 rather
than the default 0.5.

## Files

- `common.R`, `glm.R`, `RandomForest.R` — feature preparation and the R models
- `IsolationForest.ipynb` — unsupervised baseline
- `RF.ipynb` — random forest, grid-searched with H2O
- `xgboost.ipynb` — XGBoost
- `Ensemble.ipynb` — the stack and the final threshold

R, Python, H2O, XGBoost, scikit-learn.
