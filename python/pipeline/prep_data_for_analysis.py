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
        help="""Transformed file's output path, including file name.
        Ex: --output-path /home/outpath/final_file.tsv""",
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
    merged_filtered_df['GENRES'] = merged_filtered_df['GENRES'].str.upper()
    for genre in _GENRES_TO_KEEP:
        # If the movie is associated with a genre set it to 1, otherwise 0
        merged_filtered_df[genre] = \
            merged_filtered_df['GENRES'].str.contains(genre).astype(int)
    return merged_filtered_df


def export_tsv(output_path: str, merged_filtered_df: pd.DataFrame):
    # Move to utils script?
    print(f"Outputting final file to {output_path}")
    merged_filtered_df.to_csv(output_path, sep=_FILE_DELIM)


def main():
    env = _read_args(sys.argv[1:])
    merged_filtered_df = _import_join_data(env)
    merged_wide_df = _genres_to_columns(merged_filtered_df)
    export_tsv(env.outPath, merged_wide_df)


if __name__ == "__main__":
     main()