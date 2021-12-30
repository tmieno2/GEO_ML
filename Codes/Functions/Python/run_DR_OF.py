from typing import NoReturn
import econml

# Main imports
from econml.orf import DROrthoForest
from econml.sklearn_extensions.linear_model import (
    WeightedLassoCVWrapper,
    WeightedLasso,
    WeightedLassoCV,
)

# Helper imports
import numpy as np
from itertools import product
from sklearn.linear_model import (
    Lasso,
    LassoCV,
    LogisticRegression,
    LogisticRegressionCV,
)

from sklearn.ensemble import GradientBoostingRegressor


def run_DR_OF(
    Y,
    T,
    X,
    W,
    X_test,
    n_trees=1000,
    min_leaf_size=10,
    max_depth=30,
    subsample_ratio=0.7,
    lambda_reg=0.33,
    se=True,
):

    est = DROrthoForest(
        n_trees=n_trees,
        min_leaf_size=min_leaf_size,
        max_depth=max_depth,
        subsample_ratio=subsample_ratio,
        # propensity_model=LogisticRegression(
        #     C=1 / (X.shape[0] * lambda_reg), penalty="l1", solver="saga"
        # ),
        # model_Y=Lasso(alpha=lambda_reg),
        model_Y=GradientBoostingRegressor(
            n_estimators=100, min_samples_leaf=10, max_depth=30
        ),
        # propensity_model_final=LogisticRegression(
        #     C=1 / (X.shape[0] * lambda_reg), penalty="l1", solver="saga"
        # ),
        # model_Y_final=WeightedLasso(alpha=lambda_reg, max_iter=4000),
        # model_Y_final=GradientBoostingRegressor(
        #     n_estimators=100, min_samples_leaf=10, max_depth=30
        # )
    )

    est.fit(Y, T, X=X, W=W)
    treatment_effects = est.const_marginal_effect(X_test)

    if se == True:
        te_lower, te_upper = est.const_marginal_effect_interval(X_test)
        res = est.const_marginal_effect_inference(X_test)
        return treatment_effects, te_lower, te_upper, res
    else:
        return treatment_effects
