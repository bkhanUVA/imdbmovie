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
_VOTES_CUTOFF = 1000
_GENRES_FIELD = 'GENRES'
_GENRES_TO_KEEP = \
    [genre.upper() for genre in
        [
             "Documentary", "Short", "Animation", "Comedy", "Romance","Sport",
             "Action", "News", "Drama", "Fantasy", "Horror", "Biography", "Music",
             "War", "Crime", "Western", "Family", "Adventure", "History", "Mystery",
             "Sci-Fi", "Thriller",  "Musical"
        ]
    ]


class _Env(NamedTuple):
    imdbRatingsPath: str
    imdbMetadataPath: str
    outPath: str


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
    parser.add_argument(
        "--output-path",
        help="""Transformed files' output path, excluding file name.
        Ex: --output-path /home/outpath""",
        type=str,
        required=True
    )
    parsed_args = parser.parse_args(args)
    return _Env(
        imdbRatingsPath=parsed_args.imdb_ratings_file,
        imdbMetadataPath=parsed_args.imdb_metadata_file,
        outPath=parsed_args.output_path
    )


def import_tsv(input_path: str) -> pd.DataFrame:
    # Move to utils script?
    print(f"Importing data from {input_path}")
    return pd.read_csv(input_path, sep=_FILE_DELIM)


def _import_join_data(env: _Env) -> pd.DataFrame:
    ratings_df = import_tsv(env.imdbRatingsPath)
    metadata_df = import_tsv(env.imdbMetadataPath)
    merged_df = ratings_df.merge(metadata_df, how='inner', on=_PKEY)
    merged_df.columns = merged_df.columns.str.upper()
    merged_df[_GENRES_FIELD] = merged_df[_GENRES_FIELD].str.upper()
    return _drop_low_viewed_movies(merged_df)


def _drop_low_viewed_movies(merged_df: pd.DataFrame) -> pd.DataFrame:
    """Drop movies with few views to reduce noise / recommendations of
    unavailable / home movies"""
    print(f"Dropping movies with less than {_VOTES_CUTOFF} views")
    merged_filtered_df = \
        merged_df.loc[
            merged_df['NUMVOTES'] >= _VOTES_CUTOFF
        ].reset_index(drop=True)
    print(
        f"Filtering complete, went from {len(merged_df)} records to"
        f" {len(merged_filtered_df)} records"
    )
    return merged_filtered_df


def _genres_to_columns(merged_filtered_df: pd.DataFrame) -> pd.DataFrame:
    """"Take the genres column and split it into one column per genre.
    Use 1 and 0 to mark whether a movie is associated with a genre or not"""
    # Move to utils script?
    print("Transforming single genre column into multiple genre flag columns")
    merged_wide_df = merged_filtered_df.copy(deep=True)
    for genre in _GENRES_TO_KEEP:
        # If the movie is associated with a genre set it to 1, otherwise 0
        merged_wide_df[genre] = \
            merged_wide_df[_GENRES_FIELD].str.contains(genre).astype(int)
    return merged_wide_df


def _genres_to_rows(merged_filtered_df: pd.DataFrame) -> pd.DataFrame:
    """Take the genres column and create a duplicate record for each genre the
    movie is in"""
    print("Creating a record for each genre each movie is in")
    merged_long_df = merged_filtered_df.copy(deep=True)
    merged_long_df[_GENRES_FIELD] = merged_long_df[_GENRES_FIELD].str.split(",")
    merged_long_df = merged_long_df.explode(_GENRES_FIELD)
    return merged_long_df.reset_index(drop=True)


def export_tsv(
        output_path: str, output_name: str, merged_filtered_df: pd.DataFrame
):
    # Move to utils script?
    full_out_path = f'{output_path}/{output_name}.tsv'
    print(f"Outputting final file to {full_out_path}")
    merged_filtered_df.to_csv(full_out_path, sep=_FILE_DELIM)


def _export_dfs(
        output_path: str,
        merged_wide_df: pd.DataFrame,
        merged_long_df: pd.DataFrame
):
    export_tsv(output_path, 'test_wide', merged_wide_df)
    export_tsv(output_path, 'test_long', merged_long_df)


def main():
    env = _read_args(sys.argv[1:])
    merged_filtered_df = _import_join_data(env)
    merged_wide_df = _genres_to_columns(merged_filtered_df)
    merged_long_df = _genres_to_rows(merged_filtered_df)
    _export_dfs(env.outPath, merged_wide_df, merged_long_df)


if __name__ == "__main__":
     main()