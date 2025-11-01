# Makefile for Spectral Clustering Project
# Author: Chunyu Luo
# Date: 2025-11-01

R = Rscript

# Output directories
OUT_DIR = output
FIG_DIR = $(OUT_DIR)/figures

# Source files
SRC_SPECTRAL := Spectral Clustering on Concentric Shells.R
SRC_GAP      := Clusters and the Gap Statistic.R

# Escaped for Make
space := $(null) $(null)
SRC_SPECTRAL_ESC := $(subst $(space),\ ,$(SRC_SPECTRAL))
SRC_GAP_ESC      := $(subst $(space),\ ,$(SRC_GAP))
SRC_SPECTRAL_Q   := "$(SRC_SPECTRAL)"
SRC_GAP_Q        := "$(SRC_GAP)"

# Output files
SHELL_PLOT       = $(FIG_DIR)/Concentric\ Shell\ Clusters.png
PERFORMANCE_PLOT = $(FIG_DIR)/Spectral\ Clustering\ Performance\ vs\ Maximum\ Radius.png
GAP_PLOTS        = $(OUT_DIR)/Task\ 1\ Simulation\ Plots.pdf

.PHONY: all
all: install-deps setup gap spectral report

# Setup
.PHONY: setup
setup:
	@echo "=== Setting up project directories ==="
	@mkdir -p $(FIG_DIR) $(OUT_DIR)
	@echo "Directories ready."

# ----- Task 1: Gap statistic -----
.PHONY: gap
gap: $(GAP_PLOTS)
	@echo "=== Gap statistic analysis complete ==="

$(GAP_PLOTS): $(SRC_GAP_ESC)
	@echo "=== Running gap statistic simulation ==="
	$(R) $(SRC_GAP_Q)
	@echo "Gap statistic plots saved!"

# ----- Task 2: Spectral clustering -----
.PHONY: spectral
spectral: $(SHELL_PLOT) $(PERFORMANCE_PLOT)
	@echo "=== Spectral clustering analysis complete ==="

$(SHELL_PLOT) $(PERFORMANCE_PLOT): $(SRC_SPECTRAL_ESC)
	@echo "=== Running spectral clustering on concentric shells ==="
	$(R) $(SRC_SPECTRAL_Q)
	@echo "Plots saved to $(FIG_DIR)/"

# ----- Report -----
.PHONY: report
report: gap spectral
	@echo "=== Generating project summary ==="
	@{ \
	  echo "Project Summary"; \
	  echo "================"; \
	  echo; \
	  echo "Task 1: Gap Statistic Analysis"; \
	  echo "  Output: $(GAP_PLOTS)"; \
	  echo; \
	  echo "Task 2: Spectral Clustering on Concentric Shells"; \
	  echo "  Output: $(SHELL_PLOT)"; \
	  echo "  Output: $(PERFORMANCE_PLOT)"; \
	  echo; \
	  echo "All analyses completed successfully!"; \
	} > $(OUT_DIR)/summary.txt
	@cat $(OUT_DIR)/summary.txt

# ----- Clean -----
.PHONY: clean
clean:
	@echo "=== Cleaning generated artifacts ==="
	@rm -f $(OUT_DIR)/summary.txt Rplots.pdf
	@echo "Done."

.PHONY: clean-all
clean-all: clean
	@echo "=== Removing all outputs ==="
	@rm -f $(GAP_PLOTS) $(SHELL_PLOT) $(PERFORMANCE_PLOT)
	@rmdir -p $(FIG_DIR) 2>/dev/null || true
	@echo "All outputs removed."

# ----- Dependencies -----
.PHONY: install-deps
install-deps:
	@echo "=== Installing R dependencies (if missing) ==="
	$(R) -e "for(p in c('plotly','MASS','cluster','ggplot2')) if(!require(p, quietly=TRUE, character.only=TRUE)) install.packages(p, repos='https://cloud.r-project.org')"
	@echo "Dependencies OK."
