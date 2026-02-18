from pathlib import Path

import pandas as pd


def main():
    OUTDIR = Path("data/clean")
    START_DATE = "2015-01-01"
    END_DATE = "2025-01-01"

    OUTDIR.mkdir(parents=True, exist_ok=True)

    df_cw_period_date = build_cw_period_date(start_date=START_DATE, end_date=END_DATE)
    df_cw_period_date.to_csv(OUTDIR / "cw_period_date.csv", index=False)

def build_cw_period_date(start_date="2015-01-01", end_date="2025-01-01"):
    date_index = pd.date_range(start=start_date, end=end_date, freq="MS")

    df_cw_period_date = (
        pd.DataFrame({"date": date_index})
        .assign(calendar_year=lambda x: x["date"].dt.year)
        .assign(
            academic_year=lambda x: x["date"].dt.year.where(
                x["date"].dt.month >= 7, x["date"].dt.year - 1
            )
        )
        .reset_index(drop=True)
    )

    return df_cw_period_date


if __name__ == "__main__":
    main()

