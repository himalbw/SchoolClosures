import pandas as pd
import numpy as np
import janitor

from pathlib import Path

def main():
    INDIR = Path("data/raw/closures")
    INDIR_ENROLLMENT = Path("data/clean")
    INDIR_CW = Path("data/raw/crosswalks")
    OUTDIR = Path("data/clean/closures")
    YEARS = list(range(2016, 2024))

    df_closures = pd.read_csv(INDIR / "School_Overall_Shares_03.08.23.csv")
    df_enrollment_by_school = pd.read_csv(INDIR_ENROLLMENT / "school_controls.csv")
    cw_school_district = pd.read_csv(INDIR_CW / "cw_seda.csv")

    df_closures = clean_closures(df_closures)
    df_closures = add_district(df_closures, cw_school_district)
    panel = build_panel(df_closures, YEARS)

    panel.to_csv(OUTDIR / "panel_closures.csv", index=False)

def clean_closures(df_closures):
    rename_table = {
        "stateabbrev": "state",
        "ncesschoolid": "nces_school_id",
        "schoolname": "school_name"
    }
    df_closures_clean = (
        df_closures
        .clean_names()
        .rename(columns=rename_table)
        .assign(state = lambda x: x["state"].str.lower().str.strip())
        .assign(school_name = lambda x: x["school_name"].str.lower().str.strip())
        .assign(dosage = lambda x: x["share_virtual"] + x["share_hybrid"])
        [["state", "nces_school_id", "school_name", "dosage"]]
    )
    return df_closures_clean


def add_district(df_closures, cw_school_district):
    rename_table = {
        "ncessch": "nces_school_id",
        "sedasch": "seda_school_id",
        "sedaadmin": "district_id"
    }
    cw_school_district_clean = (
        cw_school_district
        .clean_names()
        .rename(columns=rename_table)
        [["nces_school_id", "seda_school_id", "district_id"]]
    )
    df_closures_with_district = (
        df_closures
        .merge(cw_school_district_clean, how="left", on="nces_school_id")
        [lambda x: ~x["district_id"].isna()]
        .drop(columns=["nces_school_id"])
    )

    return df_closures_with_district


def aggregate_to_district(df_closures, df_enrollment):

    df_enrollment_clean = df_enrollment.rename({""})

    df_closures_agg = (
        df_closures
        .merge(df_enrollment_clean, how="left", on=["seda_school_id"])
        .assign(weighted_dosage=lambda x: x["dosage"] * x["enrollment"])
        .groupby(["district_id"], as_index=False)
        .agg({"weighted_dosage": "sum", "enrollment": "sum"})
        .assign(dosage=lambda x: x["weighted_dosage"] / x["enrollment"])
        [["district_id", "dosage"]]
    )
    return df_closures_agg


def build_panel(df_closures, years):
    ids = list(df_closures["district_id"].unique())
    empty_panel = (
        pd.MultiIndex.from_product([ids, years], names=["district_id", "year"])
        .to_frame(index=False)
    )
    panel = empty_panel.merge(df_closures, how="left", on=["district_id"])
    return panel

if __name__ == "__main__":
    main()

