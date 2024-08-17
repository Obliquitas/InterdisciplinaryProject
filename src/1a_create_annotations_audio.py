from transformers import AutoModelForAudioClassification

import librosa, torch
import os
from tqdm.auto import tqdm
import pickle
import numpy as np

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(device)
# Load the model
model = AutoModelForAudioClassification.from_pretrained("3loi/SER-Odyssey-Baseline-WavLM-Multi-Attributes", trust_remote_code=True).to(device)

#get mean/std
mean = model.config.mean
std = model.config.std

results = {}

#Get all paths
root_path = "D:\\Dokumente\\emu.ZTC_BAS.2.cmdi.42072.1722336127\\ZTC_BAS_emuDB"
paths = []
for dirpath, dirnames, filenames in os.walk(root_path):
    for filename in filenames:
        if filename.endswith(".wav"):
            paths.append(os.path.join(dirpath, filename))

for path in tqdm(paths):
    dirpath, filename = os.path.split(path)

    bundle = filename.split(".")[0]
    audio_path = os.path.join(dirpath, filename)

    raw_wav, _ = librosa.load(audio_path, sr=model.config.sampling_rate)

    #split the audio into utterances
    intervals_split_on_silence = librosa.effects.split(raw_wav, top_db=30)

    #Split intervals into 1s intervals
    utterance_intervals = []
    for start, end in intervals_split_on_silence:
        for i in range(start, end, model.config.sampling_rate):
            utterance_intervals.append((i, min(i+model.config.sampling_rate, end)))

    #normalize the audio by mean/std
    norm_wav = (raw_wav - mean) / (std+0.000001)


    utterances = [norm_wav[start:end] for start, end in utterance_intervals]
    max_len = max([len(utt) for utt in utterances])
    #Make mask
    mask = torch.zeros(len(utterances), max_len)
    for i, utt in enumerate(utterances):
        mask[i, :len(utt)] = 1

    mask = mask.to(device)

    #Pad the utterances to the same length
    utterances = [torch.nn.functional.pad(torch.tensor(utt), (0, max_len-len(utt))) for utt in utterances]

    #Get dataloader for the utterances
    wavs = torch.stack(utterances).to(device)


    dataloader = torch.utils.data.DataLoader(torch.utils.data.TensorDataset(wavs, mask), batch_size=8)

    predictions = []

    for batch in dataloader:
        wavs, mask = batch
        with torch.no_grad():
            predictions.append(model(wavs, mask))

    results[bundle] = (utterance_intervals,predictions)


# Save the results
with open("results.pkl", "wb") as f:
    pickle.dump(results, f)