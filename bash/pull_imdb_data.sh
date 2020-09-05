#!/bin/bash

# Sample Command to pull all data: ./pull_imdb_data.sh -o ~/Desktop -mdr

# Defaults
out_dir=imdbmovie/data
imdb_api=https://datasets.imdbws.com


while getopts i:o:mdr flag
do
    case "${flag}" in
        i) imdb_api=${OPTARG};;
        o) out_dir=${OPTARG};;
        m) pull_movies=T;;
        d) pull_meta=T;;
        r) pull_rating=T;;
    esac
done


pull_data () {    
    echo "------------------------------------------------------------"

    if ! [ -z $1 ]
    then
        echo "STATUS: Pulling and unzipping ${2}"
        curl $imdb_api/${3} | gzip -d > $out_dir/${2}.tsv
    else
        echo "STATUS: Skipping ${2} pull"
    fi

    echo "------------------------------------------------------------"
}


echo "WARNING: this script pulls and uzips several large files"
pull_data "$pull_movies" "movies" "title.akas.tsv.gz"
pull_data "$pull_meta" "movies_metadata" "title.basics.tsv.gz"
pull_data "$pull_rating" "movies_ratings" "title.ratings.tsv.gz"
