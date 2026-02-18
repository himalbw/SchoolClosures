import pandas as pd
import numpy as np

from pathlib import Path

def main():
    INDIR = Path("data/raw/closures")
    OUTDIR = Path("data/clean/closures")


    df_closures = pd.read_csv(INDIR / "District_Overall_Shares_03.08.23.csv")





if __name__ == "__main__":
    main()
