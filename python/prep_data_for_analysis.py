#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: https://github.com/bkhanUVA
@description: Import output from pull_imdb_data.sh and prep it for analysis.
"""

import pandas as pd
import argparse
import sys
from typing import NamedTuple, List


_FILE_DELIM = '\t'
_PKEY = 'tconst'


class _Env(NamedTuple):
    imdbRatingsPath: str
    imdbMetadataPath: str


def _read_args(args: List[str]) -> NamedTuple:
    """Provide input file and path"""
    parser = argparse.ArgumentParser(
        description="Transforms input imdb data to format easier for analysis"
    )
    parser.add_argument(
        "--imdb-ratings-file", 
        help="""Ratings downloaded from IMDB.
        Ex: --input-ratings-file /home/path/movies_ratings_sample.tsv""",
        type=str,
        required=True
    )
    parser.add_argument(
        "--imdb-metadata-file",
        help="""Movie Metadata downloaded from IMDB.
        Ex: --imdb-metadata-file /home/path/movies_metadata_sample.tsv""",
        type=str,
        required=True
    )
    parsed_args = parser.parse_args(args)
    return _Env(
        imdbRatingsPath=parsed_args.imdb_ratings_file,
        imdbMetadataPath=parsed_args.imdb_metadata_file
    )


def import_tsv(input_path: str) -> pd.DataFrame:
    # Move to utils script?
    return pd.read_csv(input_path, sep=_FILE_DELIM)


def _import_join_data(env: _Env) -> pd.DataFrame:
    ratings_df = import_tsv(env.imdbRatingsPath)
    metadata_df = import_tsv(env.imdbMetadataPath)
    merged_df = ratings_df.merge(metadata_df, how='inner', on=_PKEY)
    merged_df.columns.str.upper()
    return merged_df


def main():
    env = _read_args(sys.argv[1:])
    merged_df = _import_join_data(env)
    print(merged_df)

if __name__ == "__main__":
     main()