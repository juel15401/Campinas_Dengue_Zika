"""Build the Legionnaires' update deck for Rachel Erickson (python-pptx)."""
from pptx import Presentation
from pptx.util import Inches, Pt
from pptx.dml.color import RGBColor
from pptx.enum.shapes import MSO_SHAPE
from pptx.enum.text import PP_ALIGN, MSO_ANCHOR

RED = RGBColor(0xC5, 0x05, 0x0C)      # UW Badger red
DARK = RGBColor(0x28, 0x28, 0x28)
GREY = RGBColor(0x64, 0x64, 0x64)
LIGHT = RGBColor(0xF3, 0xF1, 0xEE)
TEAL = RGBColor(0x0B, 0x6E, 0x78)
WHITE = RGBColor(0xFF, 0xFF, 0xFF)
FONT = "Calibri"

prs = Presentation()
prs.slide_width, prs.slide_height = Inches(13.333), Inches(7.5)
BLANK = prs.slide_layouts[6]


def box(slide, x, y, w, h, fill=None, line=None):
    s = slide.shapes.add_shape(MSO_SHAPE.RECTANGLE, Inches(x), Inches(y), Inches(w), Inches(h))
    if fill is None:
        s.fill.background()
    else:
        s.fill.solid(); s.fill.fore_color.rgb = fill
    if line is None:
        s.line.fill.background()
    else:
        s.line.color.rgb = line; s.line.width = Pt(1.25)
    s.shadow.inherit = False
    return s


def text(slide, x, y, w, h, paras, size=18, color=DARK, bold=False, align=PP_ALIGN.LEFT,
         anchor=MSO_ANCHOR.TOP, spacing=6):
    """paras: list of str or (str, dict) where dict may set size/bold/color/bullet/level."""
    tb = slide.shapes.add_textbox(Inches(x), Inches(y), Inches(w), Inches(h))
    tf = tb.text_frame; tf.word_wrap = True; tf.vertical_anchor = anchor
    tf.margin_left = tf.margin_right = Inches(0.05)
    for i, p in enumerate(paras):
        t, o = (p, {}) if isinstance(p, str) else p
        para = tf.paragraphs[0] if i == 0 else tf.add_paragraph()
        para.alignment = align
        para.space_after = Pt(o.get("space", spacing))
        lvl = o.get("level", 0)
        bullet = o.get("bullet", False)
        prefix = ("•  " if lvl == 0 else "–  ") if bullet else ""
        if lvl:
            para.level = 1
        # support **bold** lead-ins: "Lead: rest"
        lead = o.get("lead")
        if prefix:
            r0 = para.add_run(); r0.text = prefix
            r0.font.size = Pt(o.get("size", size)); r0.font.color.rgb = o.get("bcolor", RED); r0.font.name = FONT
        if lead:
            r1 = para.add_run(); r1.text = lead
            r1.font.bold = True; r1.font.size = Pt(o.get("size", size)); r1.font.color.rgb = o.get("color", color); r1.font.name = FONT
        r = para.add_run(); r.text = t
        r.font.size = Pt(o.get("size", size)); r.font.bold = o.get("bold", bold)
        r.font.color.rgb = o.get("color", color); r.font.name = FONT
        r.font.italic = o.get("italic", False)
    return tb


def header(slide, title, kicker=None, n=None):
    box(slide, 0, 0, 13.333, 0.12, fill=RED)
    if kicker:
        text(slide, 0.6, 0.35, 12, 0.4, [kicker.upper()], size=13, color=RED, bold=True)
    text(slide, 0.6, 0.68, 12.2, 0.9, [title], size=30, bold=True)
    if n is not None:
        text(slide, 12.2, 7.0, 0.9, 0.35, [str(n)], size=11, color=GREY, align=PP_ALIGN.RIGHT)
    text(slide, 0.6, 7.0, 9, 0.35, ["LD severity in Wisconsin · data & code update · Nov 2026"], size=11, color=GREY)


def notes(slide, s):
    slide.notes_slide.notes_text_frame.text = s


def table(slide, x, y, w, rows, col_w, size=13, header_fill=DARK, row_h=0.42, zebra=True):
    shp = slide.shapes.add_table(len(rows), len(rows[0]), Inches(x), Inches(y), Inches(w), Inches(row_h * len(rows)))
    tbl = shp.table
    for j, cw in enumerate(col_w):
        tbl.columns[j].width = Inches(cw)
    for i, row in enumerate(rows):
        for j, val in enumerate(row):
            c = tbl.cell(i, j); c.text = ""
            c.margin_left = c.margin_right = Inches(0.08); c.margin_top = c.margin_bottom = Inches(0.04)
            p = c.text_frame.paragraphs[0]; r = p.add_run(); r.text = str(val)
            r.font.size = Pt(size); r.font.name = FONT
            if i == 0:
                c.fill.solid(); c.fill.fore_color.rgb = header_fill
                r.font.bold = True; r.font.color.rgb = WHITE
            else:
                c.fill.solid(); c.fill.fore_color.rgb = LIGHT if (zebra and i % 2 == 0) else WHITE
                r.font.color.rgb = DARK
    return tbl


# ---------------------------------------------------------------- 1 title
s = prs.slides.add_slide(BLANK)
box(s, 0, 0, 13.333, 7.5, fill=DARK)
box(s, 0, 0, 0.35, 7.5, fill=RED)
text(s, 1.0, 1.6, 11, 0.5, ["THESIS PROJECT UPDATE"], size=16, color=RGBColor(0xF2, 0x9A, 0x9E), bold=True)
text(s, 1.0, 2.1, 11.3, 2.0, ["Severe Legionnaires' disease in Wisconsin, 2023–2025"], size=44, color=WHITE, bold=True)
text(s, 1.0, 3.9, 11, 1.2, ["What changed in the data and code, what we're trying to do, and what to decide next"],
     size=22, color=RGBColor(0xDD, 0xDD, 0xDD))
text(s, 1.0, 5.6, 11, 1.0, ["For Rachel Erickson · prepared by Johnny Uelmen, Uelmen Disease Ecology Lab, UW–Madison",
                            "November 2026 · ahead of the late-January committee meeting"], size=15, color=RGBColor(0xBB, 0xBB, 0xBB))
notes(s, "Purpose: get Rachel up to speed in ~10 minutes before our meeting next week.")

# ---------------------------------------------------------------- 2 the question
s = prs.slides.add_slide(BLANK); header(s, "What we're trying to answer, and why", "The project", 2)
box(s, 0.6, 1.75, 6.0, 4.9, fill=LIGHT)
text(s, 0.85, 1.9, 5.6, 4.7, [
    ("PRIMARY AIM", {"size": 13, "bold": True, "color": RED}),
    ("Among the 635 confirmed Wisconsin LD cases (2023–2025), are reported exposures — healthcare, senior "
     "living, recreational water, travel — linked to more severe outcomes once age, sex, race/ethnicity and "
     "underlying conditions are accounted for?", {"size": 17}),
    ("SECONDARY AIM (time and data permitting)", {"size": 13, "bold": True, "color": RED}),
    ("Describe where and when cases occur: population- and age-adjusted maps, seasonality, and "
     "area-level environmental and social context.", {"size": 17}),
], spacing=10)
text(s, 7.0, 1.85, 5.8, 4.9, [
    ("Why it matters", {"size": 20, "bold": True}),
    ("Case fatality is ~10% nationally, and higher for healthcare (16%) and senior-living (15%) exposures.", {"bullet": True, "size": 16}),
    ("Almost every case is hospitalized (~98%), so hospitalization tells us little. Death and length of stay are the useful measures of severity.", {"bullet": True, "size": 16}),
    ("Wisconsin now sees 200+ cases a year, and the January 2023 interview-form restructure gives three years of consistently structured WEDSS data.", {"bullet": True, "size": 16}),
    ("Findings feed DHS directly: which settings to prioritise for water-management follow-up.", {"bullet": True, "size": 16}),
], spacing=8)
notes(s, "Decided 9/24 with Frances: confirmed cases only; severity is primary; site-visit environmental data most likely out of scope.")

# ---------------------------------------------------------------- 3 what changed: access
s = prs.slides.add_slide(BLANK); header(s, "What's new: DHS cleared sharing without PII, at census tract", "Data update", 3)
text(s, 0.6, 1.65, 12.2, 0.8, ["After the Nov 3 DHS data-sharing call: you can share anything without personally identifiable information, and census tract is "
                               "the resolution to use. Code sharing is fine. So: identifiers stay inside DHS, and a de-identified tract-level file comes to me."],
     size=17, color=GREY)
# flow boxes
y = 2.85
box(s, 0.6, y, 7.6, 3.65, fill=None, line=RED)
text(s, 0.75, y + 0.08, 7.3, 0.4, ["INSIDE DHS  (Rachel runs these — about 10 minutes)"], size=13, bold=True, color=RED)
steps = [("1", "check_case_input(cases)", "Aggregate QA: duplicates, unmatched counties/tracts, date gaps"),
         ("2", "build_case_covariates(cases)", "Joins county/tract SES, access, weather, storms (no network)"),
         ("3", "make_analysis_file(cov)", "Drops identifiers; turns dates into day-counts; random study ID"),
         ("4", "county_rates(cases)", "Crude, age-adjusted and SIR rates by county, for the map")]
for i, (k, fn, d) in enumerate(steps):
    yy = y + 0.6 + i * 0.75
    c = box(s, 0.85, yy, 0.5, 0.5, fill=RED)
    text(s, 0.85, yy + 0.02, 0.5, 0.45, [k], size=18, bold=True, color=WHITE, align=PP_ALIGN.CENTER)
    text(s, 1.5, yy - 0.04, 6.6, 0.4, [fn], size=15, bold=True, color=DARK)
    text(s, 1.5, yy + 0.3, 6.6, 0.4, [d], size=12.5, color=GREY)
arrow = s.shapes.add_shape(MSO_SHAPE.RIGHT_ARROW, Inches(8.35), Inches(4.3), Inches(0.8), Inches(0.6))
arrow.fill.solid(); arrow.fill.fore_color.rgb = GREY; arrow.line.fill.background()
box(s, 9.3, y, 3.45, 3.65, fill=LIGHT)
text(s, 9.45, y + 0.08, 3.2, 3.5, [
    ("LEAVES DHS", {"size": 13, "bold": True, "color": TEAL}),
    ("De-identified file: census tract, onset year and month, age (top-coded at 90), sex, race/ethnicity, conditions, "
     "exposure, outcomes as day-counts, area covariates", {"size": 13}),
    ("Aggregate county rate table (small cells flagged)", {"size": 13}),
    ("Never leaves: names, addresses, DOB, MRN, exact dates (unless DHS OKs onset date)", {"size": 13, "bold": True}),
], spacing=9)
notes(s, "The public data (step 1) are pulled outside DHS and copied in; nothing about cases is ever sent out. "
         "Nov 3 call attendees: Frances Goglio, Jennifer Comeau, Jordan Mason (DHS), Rachel, Johnny. Rachel's summary (Teams, Nov 10): "
         "anything without PII; census tract; periodic check-ins with DHS on code and analysis.")

# ---------------------------------------------------------------- 4 fixes
s = prs.slides.add_slide(BLANK); header(s, "Problems in the November code that would have bitten us, now fixed", "Code update (v2)", 4)
rows = [["Problem in v1 (Nov 3)", "What would have happened", "Fix in v2"],
        ["Study window started 2024-01-01", "All 2023 cases (about 1/3 of the cohort) get no weather or lag data", "Window 2023-01-01 → 2025-12-31; weather from Nov 2022"],
        ["Weather cache keyed only by year", "Widening the window silently reuses a partial 2023 file", "Cache named by exact dates and counties"],
        ["Empty windows summarised as NaN / –Inf / 0", "Fake zeros (“no rain, no storms”) enter the model", "Returns NA; missing-day counter kept"],
        ["Only YYYY-MM-DD dates, only FIPS codes", "WEDSS/Excel exports (3/5/2024, county names) fail or go NA", "Parses M/D/Y, Excel and SAS dates; county names → FIPS"],
        ["Onset date required", "Cases missing onset dropped from the lag analysis", "Falls back to diagnosis → specimen → admit → report, shifted by the median gap and flagged"],
        ["No input checks", "Duplicate IDs or 2010-vintage tracts mis-join silently", "check_case_input() prints a QA report"],
        ["Output kept every column", "Identifiers would ride along into any shared file", "make_analysis_file() strips them"]]
table(s, 0.6, 1.7, 12.1, rows, [3.4, 4.5, 4.2], size=13, row_h=0.6)
notes(s, "All re-tested on synthetic cases. The public pull was re-run in full on 2026-11-13.")

# ---------------------------------------------------------------- 5 external layers
s = prs.slides.add_slide(BLANK); header(s, "Public data attached to each case (county and census tract)", "External covariates", 5)
rows = [["Layer (source)", "Examples", "Severity aim", "Spatial aim"],
        ["Social vulnerability (CDC SVI 2022; ACS with key)", "poverty, uninsured, % 65+, race/ethnicity, housing", "Confounder / effect modifier", "●"],
        ["Health status (CDC PLACES)", "smoking, COPD, diabetes, CKD prevalence", "Area proxy if WEDSS fields are sparse", "●"],
        ["NEW · Healthcare access (CMS, HRSA)", "critical-access vs acute hospitals, primary-care HPSA, km to nearest hospital", "●  delay → severity", "○"],
        ["NEW · Population by age (Census PEP)", "county × year × 5-yr age, 2020–2025", "—", "●  age-adjusted rates and SIR"],
        ["Rurality (USDA RUCC)", "metro / non-metro", "●", "●"],
        ["Weather (ERA5-Land) and storms (NOAA)", "rain, humidity, heat in the 0–14 days before onset", "○", "●  case-crossover"],
        ["Water systems (EPA SDWIS) and facilities (EPA FRS)", "surface water, cooling-tower proxies", "○", "●"]]
table(s, 0.6, 1.7, 12.1, rows, [3.6, 4.1, 2.5, 1.9], size=13, row_h=0.56)
text(s, 0.6, 6.35, 12, 0.5, ["● = main use   ○ = secondary use.   With tract cleared, exact tract values are kept; if DHS later prefers county, the code turns tract values into statewide quintiles."],
     size=12.5, color=GREY)

# ---------------------------------------------------------------- 6 WEDSS variables
s = prs.slides.add_slide(BLANK); header(s, "WEDSS fields to pull now (some aren't in the current plan)", "Variables", 6)
text(s, 0.6, 1.7, 6.1, 5.2, [
    ("Already planned", {"size": 18, "bold": True, "color": TEAL}),
    ("Age, sex, race, ethnicity", {"bullet": True, "size": 15}),
    ("Smoking (current / former / never)", {"bullet": True, "size": 15}),
    ("Chronic lung, diabetes, CKD, CVD, cancer, immunocompromise", {"bullet": True, "size": 15}),
    ("Exposures: healthcare, senior living, travel, hot tub / recreational water, cooling towers", {"bullet": True, "size": 15}),
    ("Hospitalization, death, admit and discharge dates", {"bullet": True, "size": 15}),
    ("Possibly missing: please check WEDSS", {"size": 18, "bold": True, "color": RED}),
    ("Interview status, proxy vs self, interviewer — the main source of bias", {"bullet": True, "size": 15, "lead": "Interview: "}),
    ("CDC healthcare-associated category (presumptive ≥10 days / possible), not just “any healthcare”", {"bullet": True, "size": 15, "lead": "Exposure detail: "}),
    ("Lived in long-term care before onset", {"bullet": True, "size": 15, "lead": "Residence: "}),
], spacing=5)
text(s, 6.9, 1.7, 6.0, 5.2, [
    ("", {"size": 6}),
    ("Urine antigen only vs PCR or culture. Urine antigen misses non-Lp1 strains, so test method can bias severity", {"bullet": True, "size": 15, "lead": "Diagnosis: "}),
    ("Onset, specimen, diagnosis and report dates (care-seeking delay)", {"bullet": True, "size": 15, "lead": "Timing: "}),
    ("ICU, mechanical ventilation", {"bullet": True, "size": 15, "lead": "Severity markers: "}),
    ("Admitting hospital (or its type), transfers, discharge disposition (home / SNF)", {"bullet": True, "size": 15, "lead": "Hospital: "}),
    ("Death date, and whether death was attributed to LD", {"bullet": True, "size": 15, "lead": "Death: "}),
    ("Outbreak- or cluster-associated flag", {"bullet": True, "size": 15, "lead": "Outbreak: "}),
    ("Transplant, chemotherapy, steroids, biologics (not one yes/no)", {"bullet": True, "size": 15, "lead": "Immunosuppression type: "}),
    ("Residence county, and tract if DHS geocodes it (2020 vintage)", {"bullet": True, "size": 15, "lead": "Geography: "}),
], spacing=5)
notes(s, "Interview completeness among deaths was 61% vs 85% overall — exposure is missing NOT at random. "
         "Treat 'not interviewed' as its own category, not as 'no exposure'.")

# ---------------------------------------------------------------- 7 outcomes
s = prs.slides.add_slide(BLANK); header(s, "Choosing the outcome: the “early death” problem", "Analysis plan", 7)
cards = [("Death", "~38–42 events (6%)", "Clear and clinically meaningful. But ~10 events per parameter allows only ~4 terms, so use Firth logistic regression and a lean model.", GREY),
         ("Length of stay", "~33% missing discharge dates", "People who die on day 2 look like “mild” short stays. Naive LOS rewards dying early. Use only with death as a competing risk (Fine–Gray).", GREY),
         ("Hospital-free days", "To day 30 · recommended primary", "One ordinal score: 30 minus hospital days, with death scored worst. Uses every case and both outcomes. Proportional-odds model.", RED)]
for i, (t, sub, body, col) in enumerate(cards):
    x = 0.6 + i * 4.15
    box(s, x, 1.8, 3.9, 3.3, fill=LIGHT if col != RED else None, line=col if col == RED else None)
    text(s, x + 0.2, 1.95, 3.5, 0.6, [t], size=22, bold=True, color=col if col == RED else DARK)
    text(s, x + 0.2, 2.6, 3.5, 0.5, [sub], size=14, bold=True, color=TEAL)
    text(s, x + 0.2, 3.15, 3.5, 3.2, [body], size=15.5)
text(s, 0.6, 5.4, 12.2, 0.5, ["The template script (severity_analysis_template.R) runs all three on synthetic data, so we can compare them side by side when the real file arrives."],
     size=13.5, color=GREY)
notes(s, "Committee asked: how to handle patients who die soon after admission vs. long stays. Answer: competing risks or a composite; "
         "never treat death as a short LOS.")

# ---------------------------------------------------------------- 8 power
s = prs.slides.add_slide(BLANK); header(s, "Power: death alone detects only large effects", "Power calculation", 8)
rows = [["Exposure prevalence", "OR = 1.5", "OR = 2", "OR = 3"],
        ["5% of cases (~32)", "10%", "26%", "50%"],
        ["10% of cases (~64)", "14%", "35%", "69%"],
        ["20% of cases (~127)", "20%", "44%", "85%"]]
table(s, 0.6, 1.9, 6.4, rows, [2.5, 1.3, 1.3, 1.3], size=17, row_h=0.62)
text(s, 0.6, 4.6, 6.4, 1.5, ["Simulated power (500 runs each), unadjusted logistic regression, n = 635, 6% deaths, α = 0.05. Adjusting for confounders lowers these further."],
     size=13, color=GREY)
text(s, 7.4, 1.85, 5.4, 4.8, [
    ("What this means", {"size": 20, "bold": True}),
    ("With death alone, a healthcare exposure that doubles the odds of death will usually be missed (~35% power).", {"bullet": True, "size": 16}),
    ("That favours an outcome that uses more of the data (hospital-free days), with death as a key secondary outcome reported with confidence intervals.", {"bullet": True, "size": 16}),
    ("Including 2023 (more cases) beats dropping it to get more complete outcomes, provided we impute or backfill the missing discharge dates.", {"bullet": True, "size": 16}),
    ("Next: rerun power_logistic() with the real exposure prevalences for the methods section.", {"bullet": True, "size": 16, "color": RED}),
], spacing=8)

# ---------------------------------------------------------------- 9 missing data & bias
s = prs.slides.add_slide(BLANK); header(s, "Missing data and bias: the decisions to make", "Data quality", 9)
items = [("Missing discharge dates", "30–40% missing 2023–24; ~7% in 2025 (WISH)",
          "Frances has already backfilled more 2024 dates. Open: is 2023 worth it? Missing stays are probably longer. Backfill if cheap; otherwise multiple imputation (ICU, ventilation, age, death; coded in the template). Report both."),
         ("Interview completeness", "61% among deaths vs 85% overall",
          "Keep “not interviewed” as its own exposure level. Sensitivity checks: interviewed cases only, imputed exposure, and a simple bias analysis."),
         ("Race/ethnicity and SES", "Black residents ~19% of cases vs 6–7% of WI",
          "A large disparity, so race is not just a nuisance confounder. Use individual race/ethnicity plus tract SVI, poverty and insurance; decide a priori what is confounder vs pathway. Draw the DAG."),
         ("2023 form restructure", "start of a consistent data period",
          "Check that exposure questions mean the same thing in 2023, 2024 and 2025. Include year in models.")]
for i, (t, sub, body) in enumerate(items):
    yy = 1.75 + i * 1.28
    box(s, 0.6, yy, 0.12, 1.08, fill=RED)
    text(s, 0.9, yy - 0.02, 3.5, 0.5, [t], size=17, bold=True)
    text(s, 0.9, yy + 0.42, 3.5, 0.6, [sub], size=13, color=TEAL)
    text(s, 4.5, yy + 0.02, 8.3, 1.2, [body], size=15)

# ---------------------------------------------------------------- 10 maps
s = prs.slides.add_slide(BLANK); header(s, "Maps: adjust for population and age, and mask locations", "Secondary aim", 10)
text(s, 0.6, 1.75, 6.0, 5.0, [
    ("Population-adjusted map (committee action item)", {"size": 18, "bold": True}),
    ("LD risk climbs steeply with age, so a crude rate map mostly shows where older people live.", {"bullet": True, "size": 15.5}),
    ("county_rates() returns crude, age-adjusted (US 2000 standard) and SIR rates (observed / expected from statewide age-specific rates), with exact CIs.", {"bullet": True, "size": 15.5}),
    ("Empirical-Bayes smoothed SIR stops small counties from showing extreme rates on 1–2 cases.", {"bullet": True, "size": 15.5}),
    ("Cells below 5 are flagged for suppression. Confirm DHS's threshold.", {"bullet": True, "size": 15.5}),
], spacing=7)
text(s, 6.9, 1.75, 6.0, 5.0, [
    ("Randomized-polygon / masked dot maps", {"size": 18, "bold": True}),
    ("geomask_cases.R: each case gets a random point inside its county or tract, optionally weighted to populated census blocks.", {"bullet": True, "size": 15.5}),
    ("If DHS geocodes addresses, use donut masking instead: each point moves a random distance that scales with density, stays in its county, and hides among 50 or more residents.", {"bullet": True, "size": 15.5}),
    ("Resolution: census tract (DHS, Nov 2026). Posters and public maps may still need county or masked dots; confirm with DHS.", {"bullet": True, "size": 15.5}),
], spacing=7)

# ---------------------------------------------------------------- 11 next steps
s = prs.slides.add_slide(BLANK); header(s, "To decide next week, and to do before the January meeting", "Next steps", 11)
text(s, 0.6, 1.75, 6.0, 5.2, [
    ("Questions for DHS (Frances / Jordan)", {"size": 19, "bold": True, "color": RED}),
    ("Exact onset dates: PII or not? tract + month + sex + age band will likely make nearly every case unique (k = 1)", {"bullet": True, "size": 15}),
    ("Small-cell threshold (<5 or <11) for tables and maps", {"bullet": True, "size": 15}),
    ("Is residence tract GEOID in WEDSS (2020 vintage), or only county/ZIP?", {"bullet": True, "size": 15}),
    ("Who owns the data dictionary and field-completeness table?", {"bullet": True, "size": 15}),
    ("2023 discharge-date backfill: yes or no, and by when?", {"bullet": True, "size": 15}),
    ("Can we pull the fields on slide 6 (interview status, test method, ICU, disposition)?", {"bullet": True, "size": 15}),
], spacing=7)
text(s, 6.9, 1.75, 6.0, 5.2, [
    ("Rachel, before late January", {"size": 19, "bold": True, "color": TEAL}),
    ("Write the model as an equation: outcome ~ exposure + covariates", {"bullet": True, "size": 15.5}),
    ("Confounder list and DAG; mark which are measured", {"bullet": True, "size": 15.5}),
    ("Table 1 by outcome; cases-by-month plot (24 months); LOS histogram (is it bimodal?)", {"bullet": True, "size": 15.5}),
    ("Power with real exposure prevalences (power_logistic())", {"bullet": True, "size": 15.5}),
    ("Run check_case_input() and make_analysis_file() on the extract; send me the de-identified file", {"bullet": True, "size": 15.5}),
    ("Schedule the 2nd committee meeting (after Jan 20/21)", {"bullet": True, "size": 15.5}),
], spacing=7)

# ---------------------------------------------------------------- 12 files
s = prs.slides.add_slide(BLANK); header(s, "Where everything is", "Files", 12)
rows = [["File", "What it does"],
        ["legionella_external_data.R  (v2)", "Step 1 pulls public data (no case data involved). Step 2 runs inside DHS: check → join → de-identify → rates"],
        ["severity_analysis_template.R", "Table 1, power, death model, competing risks, hospital-free days, imputation (runs on synthetic data)"],
        ["geomask_cases.R", "Randomized point-in-polygon and donut masking for maps"],
        ["output/  (public covariates)", "County and tract tables, weather 2022-11 → 2025-12, storms, hospitals, HPSA, population by age"],
        ["CHANGES_v2.md", "Line-by-line list of fixes and new variables"]]
table(s, 0.6, 1.8, 12.1, rows, [4.0, 8.1], size=14.5, row_h=0.66)
text(s, 0.6, 6.0, 12, 0.8, ["Google Drive: Rachel Erickson / Legionnaires / legionella_external_data / v2_2026-11-13 (CSVs in output/)"], size=15, bold=True, color=TEAL)

prs.save("/home/user/Campinas_Dengue_Zika/legionnaires_update/deck/LD_project_update_for_Rachel_2026-11.pptx")
print("saved")
