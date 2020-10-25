#!/bin/bash

# By bkhanUVA - https://github.com/bkhanUVA

# Sample Command to pull all data: ./pull_imdb_data.sh -o ~/Desktop -mrf

# Defaults
dt=$(date '+%Y%m%d')
out_dir=imdbmovie/data
imdb_api=https://datasets.imdbws.com
meta_test_threshold=1000000
rating_test_threshold=1000000
meta_file_name=movies_metadata_$dt
ratings_file_name=movies_ratings_$dt


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


# It is highly recommended you filter the metadata file. This filter reduces datasize by ~70% by
#  removing TV Episodes, video games, and videos, all of which should be filtered out before running analysis anyway
#  removing these data significantly decreases downstream Python / R runtimes
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


# A basic validation to see if the data was downloaded correctly
basic_validations() {
    echo "------------------------------------------------------------"

    # Only run validation if user specified for file to be downloaded
    if ! [ -z $2 ]
    then
       echo "STATUS: Starting Validations"
       file_wc=$(< $out_dir/$1.tsv wc -l)
       if [ $file_wc -ge $3 ]
       then
          echo "STATUS: $1 File Validation PASSED, file row count of $file_wc > than threshold of $3"
          # define status for passed tests to prevent missing variable error in scenarios where both tests pass 
          status=0
       else
          echo "STATUS: $1 File Validation FAILED, file row count of $file_wc < than threshold of $3"
          status=1
       fi
    fi

    if [ "$status" -eq "1" ] && ! [ -z $2 ]
    then
       echo "ERROR: $1 VALIDATIONS FAILED"
       echo "------------------------------------------------------------"
       exit 1
    elif [ "$status" -eq "0" ] && ! [ -z $2 ]
    then
       echo "STATUS: $1 VALIDATIONS PASSED"
    else
       echo "STATUS: Skipped validations for $1 as file download was skipped"
    fi
    echo "------------------------------------------------------------"
}


echo "STATUS: PULLING DATA - WARNING, this script pulls and uzips several large files"
pull_data "$pull_meta" "$meta_file_name" "title.basics.tsv.gz"
pull_data "$pull_rating" "$ratings_file_name" "title.ratings.tsv.gz"
filter_metadata "$meta_file_name"
basic_validations "$meta_file_name" "$pull_meta" "$meta_test_threshold"
basic_validations "$ratings_file_name" "$pull_rating" "$rating_test_threshold"
echo "STATUS: pull_imdb_data.sh Complete"
