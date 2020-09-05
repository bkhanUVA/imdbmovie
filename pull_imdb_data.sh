#!/bin/bash

# Sample Command to pull all data: ./pull_imdb_data.sh -o ~/Desktop -mdr

# Default output dir
out_dir=imdbmovie/data

while getopts o:mdr flag
do
    case "${flag}" in
        o) out_dir=${OPTARG};;
        m) pull_movies=T;;
        d) pull_meta=T;;
        r) pull_rating=T;;
    esac
done

echo "WARNING: this script pulls and uzips several large files"

echo "------------------------------------------------------------"

if ! [ -z "$pull_movies" ]
then
    echo "STARTED: Pulling and unzipping movies"
    curl https://datasets.imdbws.com/title.akas.tsv.gz | gzip -d > $out_dir/movies.tsv
else
    echo "WARNING: Skipping movie pull"
fi

echo "------------------------------------------------------------"

if ! [ -z "$pull_meta" ]
then
    echo "STARTED: Pulling and unzipping movie metadata"
    curl https://datasets.imdbws.com/title.basics.tsv.gz | gzip -d > $out_dir/movie_metadata.tsv
else
    echo "WARNING: Skipping movie metadata pull"
fi

echo "------------------------------------------------------------"

if ! [ -z "$pull_rating" ]
then
    echo "STARTED: Pulling and unzipping movie user ratings"
    curl https://datasets.imdbws.com/title.ratings.tsv.gz | gzip -d > $out_dir/title_ratings.tsv
else
    echo "WARNING: Skipping movie user ratings pull"
fi

echo "------------------------------------------------------------"
