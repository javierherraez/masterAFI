# -*- coding: utf-8 -*-
"""
Created on Sun Jan 31 15:47:28 2016

@author: Álvaro Barbero Jiménez
"""        

from sklearn.base import ClassifierMixin
            
class RandomForest(ClassifierMixin):
    """Custom implementation of the Random Forest model for classification
    
    Inherits from scikit-learn ClassifierMixin, so we can make use of general model methods
    such as "score".
    """
    def __init__(self, nestimators):
        """Creates a new RandomForest
        
            nestimators: number of trees in the forest
        """
        self.nestimators = nestimators
        self._estimators = []
        
    def fit(self, X, y):
        """Fits the Random Forest to a given training dataset"""
        pass  # TODO
    
    def predict(self, X):
        """Generates predictions for a given unlabeled dataset"""
        pass  # TODO


def bootstrap(X, y):
    """Returns a bootstrap sample of the given labeled dataset"""
    pass  # TODO
