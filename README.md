# SNAP × 2024 County Winners — Interactive Map (R → Self-Contained HTML)



An R script that builds a **single, self-contained HTML dashboard** mapping **2024 U.S. county presidential winners** alongside **ACS 2023 SNAP participation**. Counties are shaded by **party hue** with **fill depth by SNAP% (quantile-binned)**; the right panel displays **overlaid bars** (outer: % of all households voting BLUE/RED/OTHER; inner: % of all households **on SNAP** that voted that party). Features include state multi-select, hover tooltips, compact KPI pills, and a gear to change bin counts (2–10).



---



## Quickstart



```r

\# In R

source("build\_snap\_2024\_dashboard.R")

\# Output: snap\_2024\_dashboard.html  (open in your browser or host statically)

```



> The script embeds the requested example Census API key. Replace with your own if needed (see **Setup**).



---



## What You Get



\- **Interactive county map (Leaflet)**

- Color = county winner (customizable hues)

- Fill depth = SNAP% binned to 2–10 quantiles (default 5; change via gear ⚙️)

- **Hover tooltips**: county name, SNAP%, households, estimated SNAP households

- Selected states show **darker borders/full opacity**; unselected states are dimmed



\- **Right-side overlaid bars (Plotly)**

- **Outer bars**: % of **all households** voting Blue/Red/Other (household-weighted)

- **Inner bars**: % of **all households on SNAP** that voted that party (overlay)

- Updates live as you select states (multi-select) or **Clear** selections



\- **Dashboard pills**

- Total votes, total households on SNAP, red counties, blue counties (K/M formatting)



\- **Single HTML file** — great for GitHub Pages, Netlify, S3, or just double-click locally



---



## Data Sources



\- **SNAP participation**: ACS 2019–2023 5-year, Data Profile  

- `DP03\_0074PE` — % of households on SNAP  

- `DP02\_0001E` — total households

\- **2024 County Results**: County-level U.S. Presidential results (public CSV)

\- **Geometries**: 2023 cartographic boundaries (states \& counties), simplified for web



> Data are fetched **once at build time** and embedded in the HTML — no runtime calls.



---



## Requirements



\- R ≥ 4.2 recommended  

\- Packages (auto-installed if missing):  

`dplyr`, `sf`, `tigris`, `rmapshaper`, `tidycensus`, `readr`, `stringr`, `htmltools`, `jsonlite`, `tidyr`



Optional:

\- `renv` for reproducible environments



---



## Setup



1\. **Clone** this repo and open the project in R/RStudio.

2\. **(Optional) Use your own Census API key**

 ```r

 tidycensus::census\_api\_key("YOUR\_REAL\_KEY", install = TRUE)

 ```

 Restart R. The script also sets the key for the current session.

3\. **Build**

 ```r

 source("build\_snap\_2024\_dashboard.R")

 ```

 Open `snap\_2024\_dashboard.html` in your browser.



---



## How It Works



1\. **Shapes**: Downloads 2023 cartographic state \& county geometries (WGS84) via `tigris`; lightly simplifies with `rmapshaper`.

2\. **ACS**: Retrieves SNAP% \& household counts via `tidycensus::get\_acs` and normalizes Data Profile variable names.

3\. **Election**: Loads 2024 county results; derives `per\_dem`, `per\_gop`, `per\_other`, and `county\_winner`.

4\. **Join**: Merges ACS + election to counties; computes `snap\_hh\_est = hh\_total \* snap\_pct/100`.

5\. **Embed**: Writes state \& county GeoJSON to temp files, **reads them back as strings**, and injects them into the HTML’s JS payload.

6\. **UI**: Leaflet map + Plotly bars + KPI pills; state multi-select, **Clear** button, and **gear** to change bin count.



---



## Hosting



\- **Open locally**: double-click `snap\_2024\_dashboard.html`.

\- **GitHub Pages**: commit the HTML to the repo and enable Pages (e.g., main branch / root).

\- **Any static host**: upload the single HTML file. No server required.



---



## Customization



\### Party colors (hues)

In `build\_snap\_2024\_dashboard.R`:

```r

PALETTE <- list(

BLUE\_LIGHT = "#dbe9f6", BLUE\_DARK = "#084081",  # Dem ramp (edit as desired)

RED\_LIGHT  = "#fddbcc", RED\_DARK  = "#99000d",  # GOP ramp

PURP\_LIGHT = "#e9e4f2", PURP\_DARK = "#4a1486"   # Other ramp

)

```

Swap these for brand/accessible palettes (e.g., Okabe–Ito).



### SNAP binning (map shading)

\- Default **5** quantiles; change at runtime from **2–10** via the gear menu.  

\- Binning recalculates and recolors immediately.



\### UI text

\- Edit the header title, KPI labels, or footer note in the HTML section of the script.



---



## Reproducibility



```r

install.packages("renv")

renv::init()

\# Build once, then:

renv::snapshot()

\# On another machine:

renv::restore()

```



---



## Troubleshooting



\- **Blank page / nothing loads**  

Open the browser console (F12) and look for errors (e.g., JSON parse). This can occur if the generated HTML was edited and quotes were corrupted. Re-run the script and avoid manual edits in the HTML.



\- **No tooltips**  

Ensure you’re hovering over counties (not just borders). Tooltips show county name + SNAP% + HH + est. SNAP HH.



\- **Selector looks tall**  

It’s compact at ~100px height; adjust in the `<style>` block (`select#stateSelect`) if desired.



\- **Initial load feels heavy**  

Reduce geometry size by lowering the `keep` value:

```r

counties\_sf <- rmapshaper::ms\_simplify(counties\_sf, keep = 0.18, keep\_shapes = TRUE)

```



---





