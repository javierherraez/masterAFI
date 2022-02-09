# -*- coding: utf-8 -*-
"""
Created on Mon Dec 13 22:10:32 2021

@author: RPM6364
"""


import numpy as np
import pandas as pd

import seaborn as sns
import matplotlib.pyplot as plt

## REMUESTREO
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV

from sklearn.compose import make_column_transformer
from sklearn.preprocessing import OneHotEncoder, StandardScaler, LabelBinarizer, LabelEncoder, OrdinalEncoder
from sklearn.base import TransformerMixin

from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.pipeline import Pipeline
from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report, plot_confusion_matrix


## Leemos los datos


train  = pd.read_csv('train.csv')
df_test  = pd.read_csv('test.csv')


## Exploramos los datos

train.head()

train.describe()

list(train)

# PassengerId -> Id of Passanger, integer
# Survived -> This is the label column explaining whether the passanger survived or not, integer (0,1)
# Pclass -> Ticket class, string
# Name -> Name contains firstname, title, and lastname, string
# Sex -> Gender, string (male,female)
# Age -> Age in years, float
# SibSp -> Number of siblings/spouse abroad, integer
# Parch -> Number of parents/children, integer
# Ticket -> Ticket Number, string
# Fare -> Passanger fare, float
# Cabin -> Cabin number, string
# Embarked -> port of embarktion, string


train.hist(figsize=(16,8));

df_train,df_val = train_test_split(train, test_size=0.2, random_state=42)


pd.DataFrame({
    'Train Total': df_train.isna().sum(),
    'Train Percentage': df_train.isna().sum()*100/df_train.shape[0],
    'Validation Total': df_val.isna().sum(),
    'Validation Percentage': df_val.isna().sum()*100/df_val.shape[0],
    'Test Total': df_test.isna().sum(),
    'Test Percentage': df_test.isna().sum()*100/df_test.shape[0]
})


## La edad la vamos a imputar con la media (sigue dist. normal aprox.)
## La cabina la vamos a eliminar, ya que tiene muchos Missing values
## Fare: como hay  solo un registro con NA, lo eliminamos



# training data
df_train['Age'].fillna(df_train['Age'].mean(), inplace=True)
df_train.drop(columns='Cabin', axis=1,inplace=True)
df_train.dropna(subset=['Embarked'],axis=0, inplace=True)

# validation data
df_val['Age'].fillna(df_train['Age'].mean(), inplace=True)
df_val.drop(columns='Cabin', axis=1,inplace=True)

#test data
df_test['Age'].fillna(df_train['Age'].mean(), inplace=True)
df_test.drop(columns='Cabin', axis=1,inplace=True)
df_test.dropna(subset=['Embarked'],axis=0, inplace=True)



pd.DataFrame({
    'Train': df_train.isna().sum(),
    'Validation': df_val.isna().sum(),
    'Test': df_test.isna().sum(),
})

df_test = df_test.dropna()

## Vamos a extraer el título del nombre


for dataframe in (df_train,df_val,df_test):
    dataframe['Title'] = dataframe['Name'].apply(lambda s : s.split(',')[1].split('.')[0].strip())

df_train.head()

# Ticket, Name, and Fare features seem not that important so we can drop this column

for dataframe in (df_train,df_val,df_test):
    dataframe.drop(columns=["Name","Ticket"],inplace=True)



# Vamos a cimplificar las columnas SibSp y Parch, en una única  FamilySize
for dataframe in (df_train,df_val,df_test):
    dataframe['FamilySize'] = dataframe['SibSp'] + dataframe['Parch'] + 1

for dataframe in (df_train,df_val,df_test):
    dataframe.drop(columns=['SibSp','Parch'], inplace=True)


sns.barplot(x='Sex',y="Survived",data=df_train)

sns.barplot(x='FamilySize',y="Survived",data=df_train)

sns.barplot(x='Pclass',y="Survived",data=df_train)

sns.barplot(x='Embarked',y="Survived",data=df_train)

sns.histplot(data=df_train, x="Age", kde=True)


## Preprocesado de la info

X_train = df_train.drop(columns=['Survived','PassengerId'])
y_train = df_train['Survived']

X_val = df_val.drop(columns=['Survived','PassengerId'])
y_val = df_val['Survived']

X_test = df_test.drop(columns=['PassengerId'])
passanger_id = df_test['PassengerId']

preprocess = make_column_transformer(
    (OrdinalEncoder(),['Sex']),
    (OneHotEncoder(handle_unknown = 'ignore'),['Embarked']),
    (StandardScaler(),['Age'])
)

pipeline = Pipeline([
    ('preprocess',preprocess),
])

X_train_transformed = pipeline.fit_transform(X_train)

## Selección del modelo


# Logistic Regression
lr = LogisticRegression()
lr.fit(X_train_transformed, y_train)

# Random Forest
rf = RandomForestClassifier()
rf.fit(X_train_transformed, y_train)

# Logistic Regression
X_val_transformed = pipeline.transform(X_val)
y_val_pred = lr.predict(X_val_transformed)
lr_acc = accuracy_score(y_val_pred,y_val)
print('======Logistic Regression======')
print(classification_report(y_val, y_val_pred))

# Random Forest
X_val_transformed = pipeline.transform(X_val)
y_val_pred = rf.predict(X_val_transformed)
rf_acc = accuracy_score(y_val_pred,y_val)
print('======Random Forest======')
print(classification_report(y_val, y_val_pred))


acc_result = pd.DataFrame({
    'Logistic Regression': [lr_acc],
    'Random Forest': [rf_acc],
},index=['Score'])

acc_result.T

# logistic regression
plot_confusion_matrix(lr, X_val_transformed, y_val)  


# Random Forest
plot_confusion_matrix(rf, X_val_transformed, y_val) 

### Cross Validation

lr_score = cross_val_score(lr, X_val_transformed, y_val, cv=5).mean()
rf_score = cross_val_score(rf, X_val_transformed, y_val, cv=5).mean()


crosval_result = pd.DataFrame({
    'Logistic Regression': [lr_score],
    'Random Forest': [rf_score]},index=['Score'])

crosval_result.T

## Clasificación

X_test_transformed = pipeline.transform(X_test)
y_final = lr.predict(X_test_transformed)

df_final = pd.DataFrame({
        "PassengerId": passanger_id,
        "Survived": y_final
    })


df_final = df_final.merge(df_test, on = 'PassengerId')



## OTRA OPCIÓN PARA CV ES USAR GRID SEARCH

# rf = RandomForestClassifier(random_state = 1)
# param_grid =  {
#                 'n_estimators': np.arange(100,300,10), 
#                 'bootstrap': [False],
#                 'max_depth': np.arange(4,16,2),
#                 'max_features': ['auto'],
#                 'min_samples_leaf': [2],
#                 'min_samples_split': [2]
#               }
# clf_rf_rnd = GridSearchCV(rf, param_grid = param_grid, cv = 5, n_jobs = -1)
# best_clf_rf_rnd = clf_rf_rnd.fit(X_train,y_train)
# clf_performance(best_clf_rf_rnd,'Random Forest')

























