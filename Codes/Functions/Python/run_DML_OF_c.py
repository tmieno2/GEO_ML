from typing import NoReturn
import econml

# Main imports
from econml.orf import DMLOrthoForest
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.multioutput import MultiOutputRegressor

# Multiple continuous treatment


def run_DML_OF_c(
    Y,
    T,
    X,
    W,
    X_test,
    n_trees=1000,
    min_leaf_size=10,
    max_depth=30,
    subsample_ratio=0.7,
):

    est = DMLOrthoForest(
        model_Y=WeightedLassoCVWrapper(cv=4),
        model_T=WeightedLassoCVWrapper(cv=4),
        model_Y_final=GradientBoostingRegressor(
            n_estimators=200, max_depth=20, min_samples_leaf=20
        ),
        model_T_final=MultiOutputRegressor(
            GradientBoostingRegressor(
                n_estimators=200, max_depth=20, min_samples_leaf=20
            )
        ),
        n_trees=n_trees,
        min_leaf_size=min_leaf_size,
        max_depth=max_depth,
        subsample_ratio=subsample_ratio,
    )

    est.fit(Y, T, X=X, W=W)

    treatment_effects = est.const_marginal_effect(X_test)
    # treatment_effects = est.effect(X_test)

    return treatment_effects
