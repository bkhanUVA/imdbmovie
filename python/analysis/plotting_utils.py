#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: https://github.com/bkhanUVA
@description: Get basic stats from input created by prep_data_for_analysis.py
    and pull_imdb_data.sh
"""

import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from typing import Dict, Union, Tuple
import json
import os

_FILE_DELIM = '\t'


def import_tsv(input_path: str) -> pd.DataFrame:
    # Move to utils script? Will be duplicated multiple times
    print(f"Importing data from {input_path}")
    return pd.read_csv(input_path, sep=_FILE_DELIM)


def import_json(input_path: str) -> Dict[str, Union[str, Dict[str, int]]]:
    # Move to utils script? Will be duplicated multiple times
    print(f"Importing data from {input_path}")
    # json.loads only works on string objects, can't read file directly
    with open(input_path, 'r') as f:
        return json.loads(f.read())


def _parse_json(imdb_stats_dict: Dict[str, Union[str, Dict[str, int]]]
                ) -> Tuple[pd.DataFrame, Dict[str, int]]:

    genre_ratings_df = pd.DataFrame.from_dict(
         imdb_stats_dict['average_rating_genre'],
         orient='index',
         columns=['rating']
        )
    genre_ratings_df = genre_ratings_df.reset_index()
    genre_ratings_df = genre_ratings_df.rename(columns = {'index': 'genre'})
    return (
        genre_ratings_df,
        imdb_stats_dict['average_rating_total']
    )


# Plot 1 - Average Rating Over Time

def plot_avg_rating_by_genre(genre_ratings_df: pd.DataFrame) -> plt.axes:
    # Plot 2 - Average Rating by Genre
    sns.set(style='ticks')
    ax = sns.barplot(data=genre_ratings_df, x='genre', y='rating', color='darkblue')
    ax.set_ylim([1, 10])
    ax.set_xticklabels(ax.get_xticklabels(), rotation=40, ha="right")
    plt.xlabel('Genres', fontweight='heavy')
    plt.ylabel('Ratings', fontweight='heavy')
    return ax

# Plot 3 - Ratings Distribution

# Plot 4 - Average Rating by Runtime

# Plot 5 - User Ratings by Genre vs imdb (all users)

# Plot 6 - User Average Rating vs imdb (all users)


def main():
    # Temporary main function - delete this later
    json_file = os.path.expanduser('~/Desktop/full_imdb_stats.json')
    test_dict = import_json(json_file)
    imdb_genre_ratings_df, imdb_average_rating = _parse_json(test_dict)
    plot_avg_rating_by_genre(imdb_genre_ratings_df)
    
    

if __name__ == "__main__":
 main()