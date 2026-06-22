#  Clinical Data Exploration & Patient Outcomes Dashboard

A layered oncology RWD, visualize longitudinal clinical 
outcomes, medication regimens and tumor markers.

---

##  Repository Structure

```text
├── README.md               # App manual and metadata
├── global.R                # Data ingestion pipelines and core data frame matrices
├── app.R                   # Application configuration entrypoint (UI & Server bindings)
├── R/                      # Modular script libraries
│   ├── ObserverModule.R    # Reactivity management and active selection engines
│   ├── bar_plots.R         # Logic wrappers for plotly histogram renderers
│   ├── cachexia.R          # Vital trajectory calculators and peak scoring scripts
│   ├── timeline.R          # Timevis temporal layout script configurations
|   └── medications.R       # Parallel alluvial and Sankey tracking modules
|── www/                    # UI stylesheets and static media assets

```
---

## Local Run and Execution

1. Clone your clinical repository locally:
   ```bash
   git clone https://github.com/drrajalaxmi/OncoTracer
   cd OncoTracer
   ```
2. Open R, select the directory hosting `app.R`, and fire up the execution command:
   ```R
   shiny::runApp()
   ```

---

