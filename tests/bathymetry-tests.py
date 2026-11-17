"""Verify sign, conservative rounding and unknown/extrapolated encoding."""
import importlib.util
from pathlib import Path
import unittest

import numpy as np

spec = importlib.util.spec_from_file_location('bathymetry', Path(__file__).resolve().parents[1] / 'tools/build-bathymetry.py')
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class DepthEncoding(unittest.TestCase):
    def test_elevation_is_negated_and_rounded_down(self):
        values = module.encode(np.array([-2.349, -10.001, 3., -1000., np.nan]), np.zeros(5))
        self.assertEqual(values.tolist(), [234, 1000, 0, 32766, 65535])

    def test_uncertainty_is_retained(self):
        values = module.encode(np.array([-2., -2., -2., np.nan]), np.array([0, 1, 127, 1]))
        self.assertEqual(values.tolist(), [200, 200 | 0x8000, 200 | 0x8000, 65535])


if __name__ == '__main__':
    unittest.main()
