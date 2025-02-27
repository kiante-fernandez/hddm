from collections import OrderedDict

from hddm.models import HDDMBase


class HDDMTruncated(HDDMBase):
    def __init__(self, *args, **kwargs):
        if hasattr(self, "nn"):
            pass
        else:
            self.nn = False
        super(HDDMTruncated, self).__init__(*args, **kwargs)

    def _create_stochastic_knodes(self, include):
        knodes = OrderedDict()
        if "a" in include:
            knodes.update(
                self._create_family_trunc_normal("a", lower=1e-3, upper=1e3, value=1)
            )
        if "v" in include:
            knodes.update(self._create_family_normal("v", value=0))
        if "t" in include:
            knodes.update(
                self._create_family_trunc_normal("t", lower=1e-3, upper=1e3, value=0.01)
            )
        if "sv" in include:
            # TW: Use kabuki.utils.HalfCauchy, S=10, value=1 instead?
            knodes.update(
                self._create_family_trunc_normal("sv", lower=0, upper=1e3, value=1)
            )
            # knodes.update(self._create_family_exp('sv', value=1))
        if "sa" in include:
            knodes.update(
                self._create_family_trunc_normal("sa", lower=0, upper=1e3, value=1)
            )
        if "st" in include:
            knodes.update(
                self._create_family_trunc_normal("st", lower=0, upper=1e3, value=0.01)
            )
        if "z" in include:
            knodes.update(
                self._create_family_trunc_normal("z", lower=0, upper=1, value=0.5)
            )
        if "p_outlier" in include:
            knodes.update(
                self._create_family_trunc_normal(
                    "p_outlier", lower=0, upper=1, value=0.05
                )
            )

        return knodes
