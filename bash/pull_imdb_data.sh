#!/bin/bash

# By bkhanUVA - https://github.com/bkhanUVA

# Sample Command to pull all data: ./pull_imdb_data.sh -o ~/Desktop -mrf

# Defaults
out_dir=imdbmovie/data
imdb_api=https://datasets.imdbws.com
meta_test_threshold=1000000
rating_test_threshold=1000000

while getopts i:o:p:t:mrf flag
do
    case "${flag}" in
        i) imdb_api=${OPTARG};;
        o) out_dir=${OPTARG};;
        m) pull_meta=T;;
        r) pull_rating=T;;
        f) remove_tv=T;;
        p) meta_test_threshold=${OPTARG};;
        t) rating_test_threshold=${OPTARG};;
    esac
done

pull_data() {    
    echo "------------------------------------------------------------"

    if ! [ -z $1 ]
    then
        echo "STATUS: Pulling and unzipping ${2}"
        curl $imdb_api/${3} | gzip -d > $out_dir/${2}.tsv
    else
        echo "STATUS: Skipping ${2} pull"
    fi

    echo "STATUS: Downloads Complete"
    echo "------------------------------------------------------------"
}

filter_metadata() {
    echo "------------------------------------------------------------"

    if ! [ -z $pull_meta ] && ! [ -z $remove_tv ]
    then
        echo "Initial Metadata Row Count: $(< $out_dir/$1.tsv wc -l)"
        echo "STATUS: Removing individual TV Episodes, videos, and videogames from metadata file"
        # Title Type (movie, tv, etc) is stored in 2nd column
        awk -F "\t" '! ( $2 ~ /tvEpisode/ || $2 ~ /video/ || $2 ~ /videoGame/ )' $out_dir/$1.tsv > $out_dir/$1_filtered.tsv
        mv $out_dir/$1_filtered.tsv $out_dir/$1.tsv
        echo "Filtered Metadata Row Count: $(< $out_dir/$1.tsv wc -l)"
    else
        echo "WARNING: SKIPPING METADATA FILTER, FILE WILL HAVE ~3x MORE ROWS"
    fi

    echo "STATUS: Filtering Complete"
    echo "------------------------------------------------------------"
}

quick_validations() {
    echo "------------------------------------------------------------"
    echo "STATUS: Starting Validations"

    # Only run validation if user specified for file to be downloaded
    if ! [ -z $pull_meta ]
    then
       meta_wc=$(< $out_dir/$1.tsv wc -l)
       if [ $meta_wc -ge $meta_test_threshold ]
       then
          echo "STATUS: Metadata File Validation PASSED, file row count of $meta_wc > than threshold of $meta_test_threshold"
          # define status for passed tests to prevent missing variable error in scenarios where both tests pass 
          status=0
       else
          echo "STATUS: Metadata File Validation FAILED, file row count of $meta_wc < than threshold of $meta_test_threshold"
          status=1
       fi
    fi

    if ! [ -z $pull_rating ]
    then
       rating_wc=$(< $out_dir/$2.tsv wc -l)
       if [ $rating_wc -ge $rating_test_threshold ]
       then
          echo "STATUS: Rating File Validation PASSED, file row count of $rating_wc > than threshold of $rating_test_threshold"
          status=0
       else
          echo "STATUS: Rating File Validation FAILED, file row count of $rating_wc < than threshold of $rating_test_threshold"
          status=1
       fi
    fi

    if [ "$status" -eq "1" ]
    then
       echo "ERROR: FILE VALIDATIONS FAILED"
       echo "------------------------------------------------------------"
       exit 1
    else
       echo "STATUS: FILE VALIDATIONS PASSED"
    fi
    echo "------------------------------------------------------------"
}

echo "STATUS: PULLING DATA - WARNING, this script pulls and uzips several large files"
pull_data "$pull_meta" "movies_metadata" "title.basics.tsv.gz"
pull_data "$pull_rating" "movies_ratings" "title.ratings.tsv.gz"
filter_metadata "movies_metadata"
quick_validations "movies_metadata" "movies_ratings"
echo "STATUS: pull_imdb_data.sh Complete"
