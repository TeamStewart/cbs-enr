import clarify
import pandas as pd
import zipfile
import sys

def _flatten_result(r):
    return {
        'race_id': r.contest.key,
        'race_name': r.contest.text,
        # 'precincts_reporting': r.contest.precincts_reporting,
        # 'precincts_reported': r.contest.precincts_reported,
        'vote_mode': r.vote_type,
        'precinct_id': r.jurisdiction.name if r.jurisdiction is not None else None,
        # 'precinct_total_voters': r.jurisdiction.total_voters if r.jurisdiction is not None else None,
        # 'precinct_ballots_cast': r.jurisdiction.ballots_cast if r.jurisdiction is not None else None,
        'precinct_total': r.votes,
        'candidate_name': r.choice.text if r.choice is not None else None,
        'candidate_party': r.choice.party if r.choice is not None else None
        # 'candidate_total_votes': r.choice.total_votes if r.choice is not None else None
    }

def get_data(path):

    with zipfile.ZipFile(path, 'r') as zip_ref:
        file_names = zip_ref.namelist()
        # Assuming the file you want to parse is the first file in the archive
        file_to_parse = file_names[0]
        zip_ref.extract(file_to_parse, '../data/raw/ga/')

    p = clarify.Parser()
    p.parse(f'../data/raw/ga/{file_to_parse}')

    # Continue with the rest of your code...

    df = pd.DataFrame([_flatten_result(r) for r in p.results if r.jurisdiction is not None])
    df['jurisdiction'] = p.region

    df.to_csv(path.replace(".xml", ".csv"), index=False)