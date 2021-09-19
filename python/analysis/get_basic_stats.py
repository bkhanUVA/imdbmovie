#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: https://github.com/bkhanUVA
@description: Get basic stats from input created by prep_data_for_analysis.py
    and pull_imdb_data.sh
"""

import json
import numpy as np
import pandas as pd
from typing import Tuple, NamedTuple, List, Union, Dict
import argparse
import sys
import os

_FILE_DELIM = '\t'


class _Env(NamedTuple):
    imdbRatingsLong: str
    imdbRatingsWide: str
    outPath: str


def _read_args(args: List[str]) -> NamedTuple:
    """Provide input file and path"""
    # Move to utils script.... will be duplicated in two places
    parser = argparse.ArgumentParser(
        description="""Pulls basic stats from input created by
        prep_data_for_analysis.py and pull_imdb_data.sh"""
    )
    parser.add_argument(
        "--imdb-ratings-long", 
        help="""Input path for ratings in long format (record per genre)
        Ex: --input-ratings-long /home/path/imdb_ratings_long.tsv""",
        type=str,
        required=True
    )
    parser.add_argument(
        "--imdb-ratings-wide",
        help="""Input path for ratings in wide format (column per genre)
        Ex: --imdb-metadata-wide /home/path/imdb_ratings_wide.tsv""",
        type=str,
        required=True
    )
    parser.add_argument(
        "--output-path",
        help="""Path for final JSON containing basic imdb statistics
        Ex: --output-path /home/outpath""",
        type=str,
        required=True
    )
    parsed_args = parser.parse_args(args)
    return _Env(
        imdbRatingsLong=parsed_args.imdb_ratings_long,
        imdbRatingsWide=parsed_args.imdb_ratings_wide,
        outPath=parsed_args.output_path
    )


def import_tsv(input_path: str) -> pd.DataFrame:
    # Move to utils script? Will be duplicated multiple times
    print(f"Importing data from {input_path}")
    return pd.read_csv(input_path, sep=_FILE_DELIM)


def _import_cleaned_ratings_data(env: _Env) -> Tuple[pd.DataFrame, pd.DataFrame]:
    return import_tsv(env.imdbRatingsLong), import_tsv(env.imdbRatingsWide)


def _calculate_average_rating_by_genre(imdb_long_df: pd.DataFrame):
    mean_genres_df = \
        imdb_long_df[['GENRES', 'AVERAGERATING']].groupby('GENRES', group_keys=False).mean()   
    # Enter 'AVERAGERATING' key in-order to isolate genre values
    return mean_genres_df['AVERAGERATING'].to_dict()

def _calculate_average_rating_total(imdb_wide_df: pd.DataFrame):
    return np.nanmean(imdb_wide_df['AVERAGERATING'])


# Create object to represent nested dict....
def _generate_statistics(type: str, imdb_long_df: pd.DataFrame, imdb_wide_df: pd.DataFrame) -> Dict[str, Union[str, Dict[str, int]]]:
    stats_dict = {
        f"{type}_average_rating_genre": _calculate_average_rating_by_genre(imdb_long_df),
        f"{type}_average_rating_total": _calculate_average_rating_total(imdb_wide_df)
    }
    return stats_dict


def output_json(out_path: str, out_filename: str, stats_dict: Dict[str, Union[str, Dict[str, int]]]):
    # Move to utils
    out_path = f'{os.path.expanduser(out_path)}/{out_filename}.json'
    print(f"Exporting stats to {out_path}")
    with open(out_path, "w") as outfile: 
        json.dump(stats_dict, outfile)


def main():
    env = _read_args(sys.argv[1:])
    imdb_long_df, imdb_wide_df = _import_cleaned_ratings_data(env)
    imdb_stats_dict = _generate_statistics('imdb', imdb_long_df, imdb_wide_df)
    # Just re-use imdb dfs for now so we can test plotting service
    user_stats_dict = _generate_statistics('user', imdb_long_df, imdb_wide_df)
    # TEMP - minus 1 from user scores since we're currently reusing imdb sample
    #  to build user average as well. This will allow for easier testing of plots
    for k in user_stats_dict:
        if k == 'user_average_rating_genre':
            for k2 in user_stats_dict[k]:
                user_stats_dict[k][k2] = user_stats_dict[k][k2] - 1
        else:
            user_stats_dict[k] = user_stats_dict[k] - 1
    imdb_stats_dict.update(user_stats_dict)
    print(imdb_stats_dict)
    stats_dict = imdb_stats_dict
    output_json(env.outPath, 'full_imdb_stats', stats_dict)

if __name__ == "__main__":
     main()