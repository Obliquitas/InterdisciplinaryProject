import pickle
import pandas as pd
import torch


dfs = []


#Read results
with open("results.pkl", "rb") as f:
    results = pickle.load(f)

for key in results.keys():
    session = key[:5]
    task = key[5]
    subject = key[7:9]

    intervals, annotations = results[key]

    annotations = torch.cat(annotations, dim=0).cpu().numpy()
    
    #Make pandas dataframe
    df_intervals = pd.DataFrame()
    df_intervals["start"], df_intervals["end"] = zip(*intervals)

    #Add annotations
    df_annotations = pd.DataFrame(annotations, columns = ['arousal', 'dominance','valence'])
    df_annotations = pd.concat([df_intervals, df_annotations], axis=1)

    df_annotations["session"] = session
    df_annotations["task"] = task
    df_annotations["bundle"] = key
    df_annotations["subject"] = subject

    dfs.append(df_annotations)

df = pd.concat(dfs, ignore_index=True)
df.to_csv("data\\annotations.csv", index=False)