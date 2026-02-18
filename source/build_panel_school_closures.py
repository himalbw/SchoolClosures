import pandas as pd
import numpy as np
import janitor

from pathlib import Path

def main():
    INDIR = Path("data/raw/closures")
    INDIR_CW_PERIOD_DATE = Path("data/clean")
    INDIR_CW_SCHOOL_DISTRICT = Path("data/raw")
    OUTDIR = Path("data/clean/closures")
    YEARS = list(range(2016, 2024))

    df_closures = pd.read_csv(INDIR / "School_Overall_Shares_03.08.23.csv")
    cw_school_district = pd.read_csv(INDIR_CW_PERIOD_DATE / "cw_school_district.csv")
    cw_period_date = pd.read_csv(INDIR_CW / "cw_period_date.csv", parse_dates=["date"])

def clean_closures(df_closures):
    rename_table = {
        "stateabbrev": "state",
        "ncesschoolid": "school_id",
        "schoolname": "school_name"
    }
    df_closures_clean = (
        df_closures
        .clean_names()
        .rename(columns=rename_table)
        .assign(state = lambda x: x["state"].str.lower().strip())
        .assign(school_name = lambda x: x["school_name"].str.lower().strip())
        .assign(dosage = lambda x: x["share_virtual"] + x["share_hybrid"])
        [["state", "match_"]]
    )
    return df_closures_clean

def add_district(df_closures, cw_school_district):
    pass

def build_panel(df_closures, years=[]):
    pass

if __name__ == "__main__":
    main()

