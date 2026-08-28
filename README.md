# Power Analysis

An interactive teaching app for the one-sample z test. Move the controls and watch
the rejection region, the Type II error rate, and the power change together.


## What it shows

Two panels share a common x scale:

- **Alternative distribution**, centred at your chosen `mu_a`, split by the critical
  value into power (`1 - beta`) and the Type II error rate (`beta`).
- **Null distribution**, centred at `mu_0 = 40`, split into the significance level
  (`alpha`) and the rest of the sampling distribution.

Above them, a readout gives the current `alpha`, `beta`, power, and critical value
as numbers, which is often what you actually want to read off.

Controls cover the significance level, the alternative mean, the population standard
deviation, the sample size, and whether the test is lower-tail, upper-tail, or
two-tailed.

## Running it locally

Requires R and `shiny`. That is the only dependency.

```r
install.packages("shiny")
shiny::runApp(".")   # or: source("run_app.R")
```

## Deploying to Posit Connect Cloud

`manifest.json` describes the R version and package dependencies.

1. Push this repository to GitHub.
2. At [connect.posit.cloud](https://connect.posit.cloud), choose **New Content → Shiny → R**.
3. Select this repository with `app.R` as the entry point, then deploy.

Pushes to the default branch trigger a redeploy. Regenerate the manifest whenever
dependencies change:

```r
rsconnect::writeManifest(appDir = ".")
```

## Notes on the implementation

Drawing uses base graphics rather than ggplot2. The panels are simple shaded normal
curves, and ggplot2 carries roughly 100 ms of fixed overhead per plot regardless of
content, which dominated render time and made the sliders lag. Base graphics draw the
same picture in a few milliseconds. The grey panel and white gridlines are reproduced
directly so the appearance is unchanged.

Inputs are debounced, so dragging a slider redraws once when it settles rather than
once per step, and all test quantities are computed in a single shared reactive
instead of being recalculated inside each panel.

The layout, plot heights, and plot text scale with the viewport, so the app is usable
on a phone.

| File | Purpose |
| --- | --- |
| `app.R` | UI and server. |
| `functions.R` | Test quantities and the two plotting routines. |
| `run_app.R` | Local launcher. |
| `manifest.json` | Dependency lock file read by Posit Connect Cloud. |

## License

MIT. See [LICENSE](LICENSE).
