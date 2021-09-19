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


def _parse_json(
    imdb_stats_dict: Dict[str, Union[str, Dict[str, int]]]
) -> Tuple[pd.DataFrame, Dict[str, int]]:

    # TODO: Merge user and genre ratings imports into a single function
    genre_ratings_df = pd.DataFrame.from_dict(
         imdb_stats_dict['imdb_average_rating_genre'],
         orient='index',
         columns=['rating']
        )
    genre_ratings_df = genre_ratings_df.reset_index()
    genre_ratings_df = genre_ratings_df.rename(columns = {'index': 'genre'})
    genre_ratings_df['type'] = 'imdb'
    genre_ratings_df2 = pd.DataFrame.from_dict(
         imdb_stats_dict['user_average_rating_genre'],
         orient='index',
         columns=['rating']
        )
    genre_ratings_df2 = genre_ratings_df2.reset_index()
    genre_ratings_df2 = genre_ratings_df2.rename(columns = {'index': 'genre'})
    genre_ratings_df2['type'] = 'user'
    genre_ratings_df = pd.concat([genre_ratings_df, genre_ratings_df2])
    return (
        genre_ratings_df,
        # TODO: Clean the dictionary return logic up
        {'imdb_average_rating_total':imdb_stats_dict['imdb_average_rating_total'],
         'user_average_rating_total':imdb_stats_dict['user_average_rating_total']}
    )


# Plot 1 - Average Rating Over Time
def plot_avg_rating_by_genre(
    genre_ratings_df: pd.DataFrame
) -> plt.axes:
    # Plot 2 - Average Rating by Genre
    genre_ratings_df = genre_ratings_df.loc[genre_ratings_df['type'] == 'imdb']
    sns.set(style='ticks')
    ax = sns.barplot(data=genre_ratings_df, x='genre', y='rating', color='darkblue')
    ax.set_ylim([1, 10])
    ax.set_xticklabels(ax.get_xticklabels(), rotation=40, ha="right")
    plt.xlabel('Genres', fontweight='heavy')
    plt.ylabel('Ratings', fontweight='heavy')
    # return plt?
    return ax

# Plot 3 - Ratings Distribution

# Plot 4 - Average Rating by Runtime

# Plot 5 - User Ratings by Genre vs imdb (all users)
def plot_user_rating_vs_imdb_by_genre(
        genre_ratings_df: pd.DataFrame, average_ratings: Dict[str, float]
) -> plt.axes:
    # Plot 2 - Average Rating by Genre
    sns.set(style='ticks')
    ax = sns.catplot(data=genre_ratings_df, kind='bar', x='genre', y='rating', hue='type')
    ax.set(ylim=[1, 10])
    # ToDO Clean this up.... tilt x tick labels so they are more readable
    ax.set(title = f"IMDB Average Rating across all genres is {average_ratings['imdb_average_rating_total']}, while User Average is {average_ratings['user_average_rating_total']}")
    plt.xlabel('Genres', fontweight='heavy')
    plt.ylabel('Ratings', fontweight='heavy')
    # return plt?
    return plt

# Plot 6 - User Average Rating vs imdb (all users)


def main():
    # Temporary main function - delete this later
    json_file = os.path.expanduser('~/Desktop/full_imdb_stats.json')
    test_dict = import_json(json_file)
    imdb_genre_ratings_df, average_ratings = _parse_json(test_dict)
    plot_avg_rating_by_genre(imdb_genre_ratings_df)
    test_plt = plot_user_rating_vs_imdb_by_genre(imdb_genre_ratings_df, average_ratings)
    test_plt.savefig('test_plt.png')
    
    

if __name__ == "__main__":
 main()