# (C) Copyright 2023- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.
import matplotlib.pyplot as plt


def create_subplot_grid(N):
    # Calculate the number of rows and columns for the subplots
    if N>3:
        num_rows = int(N**0.5)
        num_cols = (N + num_rows - 1) // num_rows  # Round up

        # Create a figure and axis objects
        fig, axes = plt.subplots(num_rows, num_cols, figsize=(12, 8))

        # Flatten the axes array for easier indexing
        axes = axes.flatten()

        # Hide any empty subplots
        for i in range(N, len(axes)):
            fig.delaxes(axes[i])
    else:
        fig, axes = plt.subplots(N, figsize=(8, 12))

    # Return the figure and axes
    return fig, axes

