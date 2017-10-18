from __future__ import division

import pandas as pd
import numpy as np
import talib
from scipy.stats import zscore
from talib import MA_Type

features =8
data_path = '../data_2/^NSEBANK.csv'
input_file = pd.read_csv(data_path)
input_file = input_file.replace('null',np.nan,axis=0)
input_file = input_file.dropna(axis=0).reset_index()


open_l = input_file['Open'].values.astype(float)
high = input_file['High'].values.astype(float)
low = input_file['Low'].values.astype(float)
close = input_file['Adj Close'].values.astype(float)


# calculate TA's
close_to_sma = talib.SMA(close,timeperiod=14)
upper, middle, lower = talib.BBANDS(close, timeperiod=14,matype=MA_Type.T3)
close_to_mom = talib.MOM(close, timeperiod=14)
#macd, signal, hist = talib.MACD(close,fastperiod=12,slowperiod=26,signalperiod=9)
rsi = talib.RSI(close, timeperiod=14)
ema = talib.EMA(close, timeperiod=14)
percent = []
myList = np.arange(-1.0, 1.1, 0.1).tolist()

percent = []

for i in xrange(len(close)):
    a = ((close[i] - close[i - 1]) / close[i - 1]) * 100
    #percent.append(a)
    percent.append(min(myList, key=lambda x: abs(x - a)))

percent = np.array(percent, dtype=float)
p_list = percent.tolist()
np.savetxt("../data/percent_nifty.csv", percent)

day = [2,5,8]
for days in day:
    margins = map(str,np.arange(0.1,1.1,0.1).tolist())
    print margins
    for margin in margins:
        arr = []
        label = []
        for a in xrange(len(p_list)):
            try:
                if p_list[a + 1] >= float(margin):
                    label.append(1)
                elif ((-float(margin)) < p_list[a + 1] < float(margin)):
                    label.append(0)
                else:
                    label.append(-1)
            except IndexError:
                label.append(0)
        print len(label)
        # building and saving our data matrix
        a = close, close_to_sma, close_to_mom, upper, middle, lower, ema, rsi, np.array(label,dtype=float)#, open_l, high, low#
        a = np.column_stack(a)
        final = a[~np.isnan(a).any(axis=1)]
        df = final[:, :features]
        for i in range(days, len(df)):
            arr.append(np.append(df[i:i+1], df[i-days:i]))

        history_days = np.row_stack(arr)
        num_labels = len(label)- len(history_days)
        history_days = np.column_stack((history_days,np.array(label[num_labels:],dtype=float)))
        np.savetxt("../data_/master_data_nifty101_ta/data"+margin+"/close_ta_nifty_v3_"+str(days+1)+"day_history",history_days,delimiter=',')
        np.savetxt("../data_/master_data_nifty101_ta/data"+margin+"/close_ta_nifty_v3",final,delimiter=',')
        n_days = (features * (days + 1))
        final_load = np.loadtxt("../data_/master_data_nifty101_ta/data"+margin+"/close_ta_nifty_v3_" + str(days + 1) + "day_history", delimiter=',')

        # #normalize train_data matrix
        final_load_z = zscore(final_load[:, :n_days])

        final_load1 = np.column_stack((final_load_z, final_load[:, n_days]))
        np.savetxt("../data_/master_data_nifty101_ta/data"+margin+"/close_ta_nifty_z_v3_" + str(days + 1) + "day_history", final_load1, delimiter=',')