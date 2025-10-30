# build_snap_2024_dashboard.R
# SNAP (ACS 2023) × 2024 county winners — interactive HTML dashboard
# - Map: county hue = winner (Blue/Dem, Red/GOP, Purple/Other); fill depth = SNAP%, quantile binned (2–10; default 5)
# - Bars (right): outer = % of all households voting BLUE/RED/OTHER; inner = % of all households on SNAP & voted that party (overlay)
# - Dashboard pills above map: total votes, HH on SNAP, red counties, blue counties (K/M formatted)
# - Compact multi-select in header (top-right) + Clear button
# - Self-contained HTML; fetches data once at build time

# ---- Packages ----
pkgs <- c("dplyr","sf","tigris","rmapshaper","tidycensus","readr","stringr",
          "htmltools","jsonlite","tidyr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install)) install.packages(to_install, repos="https://cloud.r-project.org")
library(dplyr); library(sf); library(tigris); library(rmapshaper)
library(tidycensus); library(readr); library(stringr)
library(htmltools); library(jsonlite); library(tidyr)

options(tigris_use_cache = TRUE, tigris_class = "sf", stringsAsFactors = FALSE)

# ---- Config ----
OUTPUT_HTML   <- "snap_2024_dashboard.html"
ACS_YEAR      <- 2023
ACS_VAR_PCT   <- "DP03_0074PE"  # % households receiving SNAP (Data Profile)
ACS_VAR_HH    <- "DP02_0001E"   # total households (Data Profile)
ELEX_2024_URL <- "https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-24/master/2024_US_County_Level_Presidential_Results.csv"

# Census key (request a free Census API key: https://api.census.gov/data/key_signup.html )
CENSUS_API_KEY <- "ENTER-YOUR-API-HERE"
tidycensus::census_api_key(CENSUS_API_KEY, install = FALSE, overwrite = TRUE)

# ---- 1) Geometries ----
state_excl <- c("60","66","69","72","78") # AS, GU, MP, PR, VI
states_sf <- states(year=2023, cb=TRUE) |>
  st_transform(4326) |>
  filter(!STATEFP %in% state_excl) |>
  select(STATEFP, STUSPS, NAME, geometry) |>
  rename(state_name = NAME)

counties_sf <- counties(year=2023, cb=TRUE) |>
  st_transform(4326) |>
  filter(!STATEFP %in% state_excl) |>
  select(GEOID, STATEFP, NAME, geometry) |>
  rename(county_name = NAME)

# Lighter geometry for HTML
counties_sf <- rmapshaper::ms_simplify(counties_sf, keep = 0.22, keep_shapes = TRUE)

# ---- 2) ACS 2023 (robust DP handling) ----
norm_var <- function(x) sub("E$", "", x)   # DP returns without trailing E
ACS_VAR_PCT_N <- norm_var(ACS_VAR_PCT)     # "DP03_0074P"
ACS_VAR_HH_N  <- norm_var(ACS_VAR_HH)      # "DP02_0001"

acs_long <- tidycensus::get_acs(
  geography = "county",
  variables = c(ACS_VAR_PCT, ACS_VAR_HH),
  year = ACS_YEAR, survey = "acs5", geometry = FALSE
)

acs <- acs_long |>
  transmute(GEOID, variable_norm = norm_var(variable), estimate) |>
  filter(variable_norm %in% c(ACS_VAR_PCT_N, ACS_VAR_HH_N)) |>
  mutate(var_clean = ifelse(variable_norm==ACS_VAR_PCT_N, "snap_pct", "hh_total")) |>
  select(GEOID, var_clean, estimate) |>
  pivot_wider(names_from = var_clean, values_from = estimate)

stopifnot(all(c("GEOID","snap_pct","hh_total") %in% names(acs)))

# ---- 3) 2024 county election results ----
elex_raw <- readr::read_csv(ELEX_2024_URL, show_col_types = FALSE)
fips_col <- if("county_fips" %in% names(elex_raw)) "county_fips" else if("fips" %in% names(elex_raw)) "fips" else NA
stopifnot(!is.na(fips_col))

elex <- elex_raw |>
  mutate(GEOID = str_pad(as.character(.data[[fips_col]]), 5, pad = "0"),
         votes_dem  = suppressWarnings(as.numeric(.data[["votes_dem"]])),
         votes_gop  = suppressWarnings(as.numeric(.data[["votes_gop"]])),
         total_votes= suppressWarnings(as.numeric(.data[["total_votes"]])),
         per_dem    = suppressWarnings(as.numeric(.data[["per_dem"]])),
         per_gop    = suppressWarnings(as.numeric(.data[["per_gop"]]))) |>
  mutate(per_dem = ifelse(!is.na(per_dem) & per_dem > 1, per_dem/100, per_dem),
         per_gop = ifelse(!is.na(per_gop) & per_gop > 1, per_gop/100, per_gop)) |>
  mutate(per_dem   = coalesce(per_dem, ifelse(!is.na(votes_dem) & !is.na(total_votes) & total_votes>0, votes_dem/total_votes, NA_real_)),
         per_gop   = coalesce(per_gop, ifelse(!is.na(votes_gop) & !is.na(total_votes) & total_votes>0, votes_gop/total_votes, NA_real_)),
         per_other = pmax(0, 1 - coalesce(per_dem,0) - coalesce(per_gop,0))) |>
  mutate(county_winner = dplyr::case_when(
    per_dem >= per_gop & per_dem >= per_other ~ "DEM",
    per_gop >= per_dem & per_gop >= per_other ~ "GOP",
    TRUE ~ "OTHER"
  )) |>
  select(GEOID, total_votes, per_dem, per_gop, per_other, county_winner)

# ---- 4) Merge & derive ----
co <- counties_sf |>
  left_join(states_sf |> st_drop_geometry() |> select(STATEFP, STUSPS), by = "STATEFP") |>
  left_join(acs,  by = "GEOID") |>
  left_join(elex, by = "GEOID") |>
  mutate(
    snap_pct      = as.numeric(snap_pct),
    hh_total      = as.numeric(hh_total),
    total_votes   = as.numeric(total_votes),
    per_dem       = as.numeric(per_dem),
    per_gop       = as.numeric(per_gop),
    per_other     = as.numeric(per_other),
    county_winner = replace_na(county_winner, "OTHER"),
    snap_hh_est   = hh_total * (snap_pct/100)
  )

# ---- 5) GeoJSON payload (embedded) ----
states_embed <- states_sf |> select(STATEFP, STUSPS, state_name, geometry)
counties_embed <- co |>
  select(GEOID, STATEFP, STUSPS, county_name,
         snap_pct, hh_total, snap_hh_est,
         total_votes, per_dem, per_gop, per_other, county_winner,
         geometry)

tmp_states   <- tempfile(fileext = ".geojson")
tmp_counties <- tempfile(fileext = ".geojson")
st_write(states_embed,   tmp_states,   driver = "GeoJSON", quiet = TRUE)
st_write(counties_embed, tmp_counties, driver = "GeoJSON", quiet = TRUE)
states_json_text   <- paste(readLines(tmp_states, warn = FALSE),   collapse = "\n")
counties_json_text <- paste(readLines(tmp_counties, warn = FALSE), collapse = "\n")

PALETTE <- list(
  BLUE_LIGHT = "#dbe9f6", BLUE_DARK = "#084081",
  RED_LIGHT  = "#fddbcc", RED_DARK  = "#99000d",
  PURP_LIGHT = "#e9e4f2", PURP_DARK = "#4a1486"
)

# ---- 6) JS ----
js <- HTML(paste0(
  "(function(){
  // ----- Injected from R -----
  const statesGeo   = ", states_json_text, ";
  const countiesGeo = ", counties_json_text, ";
  const PALETTE     = ", toJSON(PALETTE, auto_unbox=TRUE), ";

  // ----- UI refs -----
  const stSel     = document.getElementById('stateSelect');
  const selLbl    = document.getElementById('selLabel');
  const clearBtn  = document.getElementById('clearSel');
  const gearBtn   = document.getElementById('gearBtn');
  const gearPanel = document.getElementById('gearPanel');
  const binsSel   = document.getElementById('binsSelect');
  const pillVotes = document.getElementById('pillVotes');
  const pillSnap  = document.getElementById('pillSnap');
  const pillRed   = document.getElementById('pillRed');
  const pillBlue  = document.getElementById('pillBlue');
  const barsTitle = document.getElementById('barsTitle');
  const barDiv    = document.getElementById('bars');

  // ----- States list & selection -----
  const states = statesGeo.features.map(f => ({
    fp: String(f.properties.STATEFP).padStart(2,'0'),
    abbr: f.properties.STUSPS,
    name: f.properties.state_name
  })).sort((a,b)=>a.abbr.localeCompare(b.abbr));
  states.forEach(s => stSel.add(new Option(s.abbr, s.fp)));

  const selected = new Set();
  const selectedAbbrs = () => states.filter(s => selected.has(s.fp)).map(s => s.abbr);

  // ----- Helpers -----
  const compact = n => {
    if(n==null || isNaN(n)) return '—';
    const a = Math.abs(n);
    if(a >= 1e9) return (n/1e9).toFixed(2).replace(/\\.00$/,'')+'B';
    if(a >= 1e6) return (n/1e6).toFixed(2).replace(/\\.00$/,'')+'M';
    if(a >= 1e3) return (n/1e3).toFixed(1).replace(/\\.0$/,'')+'K';
    return Math.round(n).toString();
  };

  // ----- SNAP binning for map -----
  let N_BINS = 5; binsSel.value = String(N_BINS);

  function hexToRgb(h){ const r=parseInt(h.slice(1,3),16), g=parseInt(h.slice(3,5),16), b=parseInt(h.slice(5,7),16); return [r,g,b]; }
  function rgbToHex(r,g,b){ const c=x=>('0'+x.toString(16)).slice(-2); return '#'+c(r)+c(g)+c(b); }
  function lerp(a,b,t){ return Math.round(a + (b-a)*t); }
  function makeRamp(lightHex, darkHex, bins){
    const [r1,g1,b1] = hexToRgb(lightHex), [r2,g2,b2] = hexToRgb(darkHex);
    const arr = [];
    for(let i=0;i<bins;i++){
      const t = bins===1 ? 0 : i/(bins-1);
      arr.push(rgbToHex(lerp(r1,r2,t), lerp(g1,g2,t), lerp(b1,b2,t)));
    }
    return arr;
  }
  function getPartyRamp(party){
    if(party==='DEM') return makeRamp(PALETTE.BLUE_LIGHT, PALETTE.BLUE_DARK, N_BINS);   // teal
    if(party==='GOP') return makeRamp(PALETTE.RED_LIGHT,  PALETTE.RED_DARK,  N_BINS);   // orange
    return makeRamp(PALETTE.PURP_LIGHT, PALETTE.PURP_DARK, N_BINS);                     // purple
  }

  const ALL_SNAP = countiesGeo.features.map(f => +f.properties.snap_pct || 0);
  function computeBreaks(values, bins){
    const v = values.filter(x=>x!=null && !isNaN(x)).slice().sort((a,b)=>a-b);
    if(v.length===0) return Array.from({length:bins+1},(_,i)=>0);
    const q = p => {
      const idx = (v.length-1)*p;
      const lo = Math.floor(idx), hi = Math.ceil(idx);
      if(lo===hi) return v[lo];
      const w = idx-lo;
      return v[lo]*(1-w) + v[hi]*w;
    };
    const br = [q(0)];
    for(let k=1;k<bins;k++) br.push(q(k/bins));
    br.push(q(1));
    return br;
  }
  let BREAKS = computeBreaks(ALL_SNAP, N_BINS);

  function snapBin(pct){
    if(pct==null || isNaN(pct)) return null;
    for(let i=BREAKS.length-2;i>=0;i--){ if(pct>=BREAKS[i]) return i; }
    return 0;
  }
  function countyFillColor(winner, snapPct){
    const bin = snapBin(+snapPct);
    if(bin==null) return '#f0f0f0';
    const ramp = getPartyRamp(winner);
    const idx  = Math.max(0, Math.min(ramp.length-1, bin));
    return ramp[idx];
  }

  // ----- Map -----
  const map = L.map('map', {center:[37.8,-96], zoom:4, preferCanvas:true, zoomSnap:0.25});
  L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
    {attribution:'© OpenStreetMap contributors'}).addTo(map);

  // State borders (dynamic style)
  const stateBorder = L.geoJSON(statesGeo, {
    style: f => {
      const fp = String(f.properties.STATEFP).padStart(2,'0');
      const sel = selected.size===0 || selected.has(fp);
      return { color: sel ? '#111' : '#777', weight: sel ? 2.2 : 1, fill:false, opacity:1 };
    }
  }).addTo(map);

  // Counties (hover tooltip + click-to-toggle state)
  const countyLayer = L.geoJSON(countiesGeo, {
    style: f => {
      const fp = String(f.properties.STATEFP).padStart(2,'0');
      const isSelState = (selected.size===0 || selected.has(fp));
      return {
        fillColor: countyFillColor(f.properties.county_winner, f.properties.snap_pct),
        fillOpacity: isSelState ? 0.92 : 0.25,
        color: isSelState ? '#666' : '#aaa',
        weight: isSelState ? 0.5 : 0.25
      };
    },
    onEachFeature: (feature, layer) => {
      const p = feature.properties;
      const snapPct = (p.snap_pct!=null && !isNaN(p.snap_pct)) ? p.snap_pct.toFixed(1)+'%' : 'n/a';
      layer.bindTooltip(
        `<b>${p.county_name || 'County'}</b>, ${p.STUSPS}<br>`+
        `SNAP: ${snapPct}`,
        {sticky:true, direction:'auto'}
      );
      layer.on('click', () => {
        const fp = String(p.STATEFP).padStart(2,'0');
        if(selected.has(fp)) selected.delete(fp); else selected.add(fp);
        for(const opt of stSel.options){ if(opt.value===fp) opt.selected = selected.has(fp); }
        updateAll();
      });
    }
  }).addTo(map);

  // ----- Selection events -----
  stSel.addEventListener('change', ()=>{
    selected.clear();
    Array.from(stSel.selectedOptions).forEach(o => selected.add(o.value));
    updateAll();
  });
  clearBtn.addEventListener('click', ()=>{
    selected.clear();
    for(const opt of stSel.options) opt.selected = false;
    updateAll();
  });
  gearBtn.addEventListener('click', ()=>{
    gearPanel.style.display = (gearPanel.style.display==='block') ? 'none' : 'block';
  });
  binsSel.addEventListener('change', ()=>{
    const nb = Math.max(2, Math.min(10, parseInt(binsSel.value||'5',10)));
    N_BINS = nb;
    BREAKS = computeBreaks(ALL_SNAP, N_BINS);
    refreshStyles();
  });

  // ----- Bars (overlay: outer voting share, inner SNAP-in-party) -----
  function featsForSelection(){
    if(selected.size===0) return countiesGeo.features;
    return countiesGeo.features.filter(f => selected.has(String(f.properties.STATEFP).padStart(2,'0')));
  }
  function updateSelLabel(){
    selLbl.textContent = selected.size===0 ? 'USA' : selectedAbbrs().join(', ');
    barsTitle.textContent = selLbl.textContent;
  }
  function updatePills(){
    const feats = featsForSelection();
    let totalVotes = 0, snapHH = 0, red = 0, blue = 0;
    feats.forEach(f=>{
      const p = f.properties;
      totalVotes += (+p.total_votes || 0);
      snapHH     += (+p.snap_hh_est || 0);
      if(p.county_winner==='GOP') red++;
      if(p.county_winner==='DEM') blue++;
    });
    pillVotes.textContent = compact(totalVotes);
    pillSnap.textContent  = compact(Math.round(snapHH));
    pillRed.textContent   = compact(red);
    pillBlue.textContent  = compact(blue);
  }
  function computeOverlay(){
    const feats = featsForSelection();
    let HH = 0;
    const acc = { DEM:{hh:0, snap_hh:0}, GOP:{hh:0, snap_hh:0}, OTHER:{hh:0, snap_hh:0} };
    feats.forEach(f=>{
      const p = f.properties;
      const H = +p.hh_total || 0;
      const S = +p.snap_pct || 0;
      const d = +p.per_dem   || 0;
      const r = +p.per_gop   || 0;
      const o = +p.per_other || Math.max(0, 1 - d - r);
      HH += H;
      acc.DEM.hh     += H*d;          acc.DEM.snap_hh   += H*d*(S/100);
      acc.GOP.hh     += H*r;          acc.GOP.snap_hh   += H*r*(S/100);
      acc.OTHER.hh   += H*o;          acc.OTHER.snap_hh += H*o*(S/100);
    });
    const pct = x => (HH>0 ? 100*x/HH : 0);
    return {
      outer:{ DEM:pct(acc.DEM.hh), GOP:pct(acc.GOP.hh), OTHER:pct(acc.OTHER.hh) },
      inner:{ DEM:pct(acc.DEM.snap_hh), GOP:pct(acc.GOP.snap_hh), OTHER:pct(acc.OTHER.snap_hh) }
    };
  }
  function partyColor(party, depth){
    if(party==='DEM') return depth ? PALETTE.BLUE_DARK : PALETTE.BLUE_LIGHT;
    if(party==='GOP') return depth ? PALETTE.RED_DARK  : PALETTE.RED_LIGHT;
    return depth ? PALETTE.PURP_DARK : PALETTE.PURP_LIGHT;
  }
  function renderBars(){
    const st   = computeOverlay();
    const x    = ['Blue','Red','Other'];
    const outer= [st.outer.DEM, st.outer.GOP, st.outer.OTHER];
    const inner= [st.inner.DEM, st.inner.GOP, st.inner.OTHER];

    const outerTrace = {
      type:'bar', x, y: outer,
      marker:{color:[partyColor('DEM',false),partyColor('GOP',false),partyColor('OTHER',false)], line:{width:0.5, color:'#444'}},
      opacity:0.55, name:'% households voting party', hovertemplate:'%{y:.1f}% of households<extra></extra>'
    };
    const innerTrace = {
      type:'bar', x, y: inner,
      marker:{color:[partyColor('DEM',true),partyColor('GOP',true),partyColor('OTHER',true)]},
      opacity:0.95, name:'% households on SNAP & voted party', hovertemplate:'%{y:.1f}% of households<extra></extra>'
    };

    const layout = {
      barmode:'overlay',
      margin:{l:54,r:16,t:10,b:46},
      yaxis:{range:[0,100], title:'Percent of households'},
      xaxis:{title:''},
      showlegend:true, legend:{orientation:'h', y:-0.18}
    };

    Plotly.react(barDiv, [outerTrace, innerTrace], layout, {displayModeBar:false});
  }

  function refreshStyles(){
    countyLayer.setStyle(f => {
      const fp = String(f.properties.STATEFP).padStart(2,'0');
      const isSelState = (selected.size===0 || selected.has(fp));
      return {
        fillColor: countyFillColor(f.properties.county_winner, f.properties.snap_pct),
        fillOpacity: isSelState ? 0.92 : 0.25,
        color: isSelState ? '#666' : '#aaa',
        weight: isSelState ? 0.5 : 0.25
      };
    });
    stateBorder.setStyle(f => {
      const fp = String(f.properties.STATEFP).padStart(2,'0');
      const sel = selected.size===0 || selected.has(fp);
      return { color: sel ? '#111' : '#777', weight: sel ? 2.2 : 1, fill:false, opacity:1 };
    });
  }

  function updateAll(){ updateSelLabel(); updatePills(); renderBars(); refreshStyles(); }
  updateAll();
})();"
))



# ---- 7) HTML shell ----
page <- tags$html(
  tags$head(
    tags$meta(charset="utf-8"),
    tags$title("SNAP × 2024 — Map + Overlaid Bars"),
    tags$link(rel="stylesheet", href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"),
    tags$style(HTML("
      :root{
        --bg:#f6f8fb; --card:#ffffff; --ink:#0b1f33; --muted:#5b6b7c; --line:#dde5ef;
        --brand:#0A2A4D; --chip:#eef3f9;
      }
      *{box-sizing:border-box}
      body{margin:0; background:var(--bg); color:var(--ink); font-family:Inter,system-ui,Segoe UI,Roboto,Helvetica,Arial,sans-serif;}
      #wrap{max-width:1400px; margin:0 auto; padding:14px 14px 20px;}
      #header{display:flex; align-items:center; justify-content:space-between;
              background:var(--brand); color:#fff; padding:10px 12px; border-radius:14px; box-shadow:0 1px 12px #0A2A4D22;}
      #hdrLeft{display:flex; align-items:center; gap:10px;}
      #hdrLeft h1{font-size:16px; margin:0; font-weight:700; letter-spacing:0.2px;}
      #hdrRight{display:flex; align-items:center; gap:8px;}
      label.hlbl{font-weight:600; font-size:12px; opacity:.9;}
      select#stateSelect{
        width:180px; max-width:180px; height:100px;
        padding:4px 6px; border:1px solid var(--line); border-radius:8px; background:#fff;
        font-size:12px; line-height:1.15;
      }
      #selLabel{padding:4px 8px; background:var(--chip); border-radius:10px; font-weight:700; color:#fff; background:#1f6feb;}
      .btn{cursor:pointer; border:1px solid #cbd5e1; background:#fff; border-radius:8px; padding:5px 8px; font-size:12px; color:#0b1f33;}
      .btn:hover{background:#f5f7fb}
      #gearPanel{
        display:none; position:absolute; right:14px; top:66px; z-index:999;
        background:var(--card); border:1px solid var(--line); border-radius:12px; padding:10px 12px; box-shadow:0 6px 18px #0000001c;
      }
      #gearPanel h3{margin:4px 0 8px; font-size:14px;}
      #binsSelect{width:70px; padding:4px 6px; border:1px solid var(--line); border-radius:8px;}

      #pills{display:flex; gap:10px; margin:12px 0 8px 0; flex-wrap:wrap;}
      .pill{background:#fff; border:1px solid var(--line); border-radius:12px; padding:8px 12px; box-shadow:0 1px 8px #0001; min-width:150px;}
      .pill .lbl{font-size:12px; color:#556; margin-bottom:4px;}
      .pill .val{font-size:18px; font-weight:800;}

      #main{display:flex; gap:12px;}
      #mapBox{flex:2 1 760px;}
      #barBox{flex:1 1 380px;}
      #map{height:680px; border-radius:12px; overflow:hidden; box-shadow:0 1px 12px #00000014; background:#fff;}
      #barsCard{background:#fff; border:1px solid var(--line); border-radius:12px; box-shadow:0 1px 12px #00000014; padding:8px;}
      #barsTitle{font-weight:800; padding:6px 8px 0 8px;}
      #bars{height:630px; padding:0 4px 8px 4px;}
      .footnote{margin-top:6px; font-size:12px; color:var(--muted); background:#fff; border:1px solid var(--line);
                border-radius:10px; padding:6px 8px; box-shadow:0 1px 6px #0000000d;}
      @media (max-width: 980px){ #main{flex-direction:column;} #map{height:560px;} #bars{height:420px;} }
    "))
  ),
  tags$body(
    div(id="wrap",
        div(id="header",
            div(id="hdrLeft",
                tags$h1("SNAP × 2024 — Counties & Households"),
                span("Selection: "), span(id="selLabel","USA")
            ),
            div(id="hdrRight",
                tags$label(class="hlbl","States"),
                tags$select(id="stateSelect", multiple="multiple"),
                tags$button(id="clearSel", class="btn", "Clear"),
                tags$button(id="gearBtn", class="btn", HTML("&#9881;"))
            ),
            div(id="gearPanel",
                tags$h3("SNAP shading bins"),
                tags$label("Number of bins"),
                tags$select(id="binsSelect", lapply(2:10, function(i) tags$option(value=i, as.character(i))))
            )
        ),
        
        div(id="pills",
            div(class="pill", span(class="lbl","Total Votes"), span(id="pillVotes", class="val","—")),
            div(class="pill", span(class="lbl","HH on SNAP"),  span(id="pillSnap",  class="val","—")),
            div(class="pill", span(class="lbl","Red Counties"), span(id="pillRed",   class="val","—")),
            div(class="pill", span(class="lbl","Blue Counties"),span(id="pillBlue",  class="val","—"))
        ),
        
        div(id="main",
            div(id="mapBox",
                div(id="map"),
                div(class="footnote","Hover a county to see SNAP%, Households, and SNAP households. Map shading = party hue × SNAP% depth (ACS 2023).")
            ),
            div(id="barBox",
                div(id="barsCard",
                    div(id="barsTitle","USA"),
                    div(id="bars")
                )
            )
        )
    ),
    tags$script(src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"),
    tags$script(src="https://cdn.plot.ly/plotly-2.30.0.min.js"),
    tags$script(js)
  )
)


save_html(page, file = OUTPUT_HTML)
message("✅ Wrote: ", normalizePath(OUTPUT_HTML))
