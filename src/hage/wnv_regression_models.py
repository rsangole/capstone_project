import pathlib
import numpy as np
from keras.models import Sequential
from keras.layers import Bidirectional, LSTM
from keras.layers.core import Dense, Dropout, Activation
from keras.callbacks import EarlyStopping, TensorBoard
from keras.wrappers.scikit_learn import KerasRegressor
from keras.utils import np_utils
from sklearn import metrics
from sklearn.linear_model import Ridge
from sklearn.model_selection import KFold, cross_val_score, GridSearchCV
from sklearn.preprocessing import StandardScaler, MinMaxScaler, LabelEncoder
from sklearn.ensemble import RandomForestRegressor
from sklearn.pipeline import Pipeline
import pandas as pd
current_dir = pathlib.Path.cwd()
repopath = pathlib.Path(current_dir).resolve().parents[0]
datapath = repopath / pathlib.Path('data/processed')
fulldatpath = datapath / pathlib.Path('wnv.trap.date.rev3b.csv')

outpath = datapath.resolve().parents[0] / pathlib.Path('output')

filepath = datapath / pathlib.Path("wnv.trap.date.rev3b.csv")
scaler = StandardScaler()
le = LabelEncoder()
dat = pd.read_csv(filepath, parse_dates = ['t_date'])



def get_relevant_columns(df, colname):
    cols = list()
    for col in df.columns.tolist():
        if (not 'mos_' in col) & (col != colname) & (df[col].dtypes != 'object') & (df[col].dtypes != '<M8[ns]'):
            cols.append(col)
    cols.append(colname)
    return cols

def make_x(df, colname):
    cols = get_relevant_columns(df, colname)
    scaler.fit(dat[cols])
    train_x = scaler.transform(df.loc[df.part_train == True, cols])
    train_y = df.loc[df.part_train == True, colname]
    valid_x = scaler.transform(df.loc[df.part_validate == True, cols])
    valid_y = df.loc[df.part_validate == True, colname]
    test_x = scaler.transform(df.loc[df.part_test == True, cols])
    test_y = df.loc[df.part_test == True, colname]
    return train_x, train_y, valid_x, valid_y, test_x, test_y

def ridge_model(X, y, test_X, test_Y, colname):
    ridge = Ridge()
    ridge_params = {'alpha': [.0001, .001, .01, .1, .5, 1],
                    'normalize': [True, False],
                    'tol': [.0001, .001, .01], 'solver': ['auto', 'svd', ]}
    ridge = GridSearchCV(estimator=ridge, param_grid=ridge_params, n_jobs=1)
    ridge.fit(X, y)
    preds_out = pd.DataFrame(test_Y, )
    preds_out['predicted_' + colname + '_mosquitos'] = ridge.predict(test_X)
    #     preds_out.to_csv(outpath / pathlib.Path('regr_ridge_' + colname + '_predictions.csv'), index = False)
    return ridge

def rf_model(X, y, test_X, test_Y, colname):
    rf = RandomForestRegressor()
    rf_params = {'n_estimators': [100, 500, 1000, ],
                 'max_depth': [2, 3, 4],
                 'min_samples_leaf': [1, 2, 3],
                 'max_features': ['auto', ]}
    rf = GridSearchCV(estimator=rf, param_grid=rf_params, )
    rf.fit(X, y)
    preds_out = pd.DataFrame(test_Y, )
    preds_out['predicted_' + colname + '_mosquitos'] = rf.predict(test_X)
    #     preds_out.to_csv(outpath / pathlib.Path('regr_randomforest_' + colname + '_predictions.csv'), index = False)
    return rf

def basic_nnet(X, y, test_X, test_Y, colname):
    earlystopper = EarlyStopping(monitor='loss', min_delta=0, patience=3, verbose=0,
                                 mode='auto', baseline=None, restore_best_weights=True)

    model = Sequential()
    model.add(Dense(50, input_dim=X.shape[1], kernel_initializer='normal', activation='relu'))
    model.add(Dense(50, activation='relu'))
    model.add(Dense(25, activation='relu'))
    model.add(Dense(25, activation='relu'))
    model.add(Dense(10, activation='relu'))
    model.add(Dense(1))
    model.compile(optimizer='adam', loss='mse')
    model.fit(X, y, epochs=100, validation_split= 0.1, callbacks = [earlystopper, ])
    preds_out = pd.DataFrame(test_Y, )
    preds_out['predicted_' + colname + '_mosquitos'] = model.predict(test_X)
    #     preds_out.to_csv(outpath / pathlib.Path('basic_neural_net_' + colname + '_predictions.csv'), index = False)
    return model

def LSTM_model(X, y, test_X, test_Y, colname):
    earlystopper = EarlyStopping(monitor='loss', min_delta=0, patience=3, verbose=0,
                                 mode='auto', baseline=None, restore_best_weights=True)

    X = pd.DataFrame(X)
    X = pd.concat([X, X.shift(1), X.shift(2), X.shift(3)], axis=1).fillna(0).values
    X = X.reshape((X.shape[0], 1, X.shape[1]))
    model = Sequential()
    model.add(LSTM(32, input_shape=(X.shape[1], X.shape[2]), return_sequences=True))
    model.add(Dropout(0.2))
    model.add(LSTM(32, input_shape=(X.shape[1], X.shape[2]), return_sequences=True))
    model.add(Dropout(0.2))
    model.add(LSTM(32, input_shape=(X.shape[1], X.shape[2]), return_sequences=True))
    model.add(Dropout(0.2))
    model.add(LSTM(32, input_shape=(X.shape[1], X.shape[2])))
    model.add(Dropout(0.2))
    model.add(Dense(1))
    model.compile(loss='mae', optimizer='adamax')
    history = model.fit(X, y, epochs=200, batch_size=10, shuffle=False, verbose=2, validation_split= 0.1, callbacks = [earlystopper, ])
    return model

mos_cols = ['mos_erraticus_num_mosquitos','mos_pipiens_num_mosquitos','mos_pipiens_restuans_num_mosquitos',
            'mos_restuans_num_mosquitos','mos_salinarius_num_mosquitos', 'mos_tarsalis_num_mosquitos',
            'mos_territans_num_mosquitos',  'mos_unspecified_num_mosquitos','mos_tot_num_mosquitos']

# Make predictions for validation set

for mos in mos_cols:
    train_x, train_y, valid_x, valid_y, test_x, test_y = make_x(dat, mos)
    outdf = dat.loc[dat.part_validate == True, ['t_date', 'loc_lat', 'loc_lng', 'trap_trap_name', mos]]
    ridge = ridge_model(train_x, train_y, valid_x, valid_y, mos)
    outdf['ridgepreds'] = ridge.predict(valid_x)
    nnet = basic_nnet(train_x, train_y, valid_x, valid_y, mos)
    outdf['neuralnetpreds'] = nnet.predict(valid_x)
    rf = rf_model(train_x, train_y, valid_x, valid_y, mos)
    outdf['rf_preds'] = rf.predict(valid_x)
    test_X = pd.DataFrame(valid_x)
    test_X = pd.concat([test_X, test_X.shift(1), test_X.shift(2), test_X.shift(3)], axis = 1).fillna(0).values
    test_X = test_X.reshape((test_X.shape[0], 1, test_X.shape[1]))
    lstm_ = LSTM_model(train_x, train_y, test_X, valid_y, mos)
    outdf['lstm_preds'] = lstm_.predict(test_X)
    savepath = outpath / pathlib.Path('assembled_predictions_{}_mosquitos.csv'.format(mos))
    outdf.to_csv(savepath, index = False)


# Make predictions for test set

for mos in mos_cols:
    train_x, train_y, valid_x, valid_y, test_x, test_y = make_x(dat, mos)
    outdf = dat.loc[dat.part_test == True, ['t_date', 'loc_lat', 'loc_lng', 'trap_trap_name', mos]]
    ridge = ridge_model(train_x, train_y, test_x, test_y, mos)
    outdf['ridgepreds'] = ridge.predict(test_x)
    nnet = basic_nnet(train_x, train_y, test_x, test_y, mos)
    outdf['neuralnetpreds'] = nnet.predict(test_x)
    rf = rf_model(train_x, train_y, test_x, test_y, mos)
    outdf['rf_preds'] = rf.predict(test_x)
    test_X = pd.DataFrame(test_x)
    test_X = pd.concat([test_X, test_X.shift(1), test_X.shift(2), test_X.shift(3)], axis = 1).fillna(0).values
    test_X = test_X.reshape((test_X.shape[0], 1, test_X.shape[1]))
    lstm_ = LSTM_model(train_x, train_y, test_X, test_y, mos)
    outdf['lstm_preds'] = lstm_.predict(test_X)
    savepath = outpath / pathlib.Path('assembled_predictions_testset_{}_mosquitos.csv'.format(mos))
    outdf.to_csv(savepath, index = False)