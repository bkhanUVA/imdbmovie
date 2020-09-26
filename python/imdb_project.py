#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
@author: https://github.com/bkhanUVA
"""
import pandas as pd

input_dir = '~/Desktop/geu/Other_Stuff/imdbmovie/data'

movie_ratings = pd.read_csv(f'{input_dir}/movies_ratings_sample.tsv', sep='\t')
movie_metadata = pd.read_csv(f'{input_dir}/movies_metadata_sample.tsv', sep='\t')
