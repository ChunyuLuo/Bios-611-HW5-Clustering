# Makefile for Spectral Clustering Project
# Author: Chunyu Luo
# Date: 2025-11-01

R = Rscript

export RSTUDIO_PANDOC = /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools

OUT_DIR = output
FIG_DIR = $(OUT_DIR)/figures

SRC_GAP = Clusters and the Gap Statistic.R
SRC_SPECTRAL = Spectral Clustering on Concentric Shells.R

GAP_PLOT = $(FIG_DIR)/Gap_Statistic_Simulation.png
SHELL_PLOT = $(FIG_DIR)/Concentric Shell Clusters.png
PERFORMANCE_PLOT = $(FIG_DIR)/Spectral Clustering Performance vs Maximum Radius.png

.PHONY: all
all: install-deps setup gap spectral report

.PHONY: setup
setup:
	@echo "=== Setting up project directories ==="
	@mkdir -p $(FIG_DIR)
	@echo "Directories ready."

.PHONY: gap
gap: setup
	@echo "=== Running gap statistic simulation ==="
	@$(R) "$(SRC_GAP)"
	@if [ -f "$(GAP_PLOT)" ]; then \
		echo "Gap statistic plot saved successfully!"; \
	else \
		echo "ERROR: Gap plot was not generated!"; \
		exit 1; \
	fi

.PHONY: spectral
spectral: setup
	@echo "=== Running spectral clustering on concentric shells ==="
	@$(R) "$(SRC_SPECTRAL)"
	@if [ -f "$(SHELL_PLOT)" ] && [ -f "$(PERFORMANCE_PLOT)" ]; then \
		echo "Spectral clustering plots saved successfully!"; \
	else \
		echo "ERROR: Plots were not generated!"; \
		exit 1; \
	fi

.PHONY: clean
clean:
	@echo "=== Cleaning generated artifacts ==="
	@rm -f $(OUT_DIR)/summary.txt
	@rm -f Rplots.pdf
	@echo "Done."

.PHONY: clean-all
clean-all:
	@echo "=== Removing all outputs ==="
	@rm -rf $(OUT_DIR)
	@rm -f Rplots.pdf
	@echo "All outputs removed."

.PHONY: install-deps
install-deps:
	@echo "=== Installing R dependencies ==="
	@$(R) -e "pkgs <- c('plotly', 'MASS', 'cluster', 'htmlwidgets', 'reticulate'); \
		for(p in pkgs) { \
			if(!require(p, quietly=TRUE, character.only=TRUE)) { \
				install.packages(p, repos='https://cloud.r-project.org'); \
			} \
		}"
	@echo "=== Installing Python kaleido for plotly export ==="
	@pip3 install kaleido 2>/dev/null || pip install kaleido 2>/dev/null || echo "Note: kaleido installation failed, will use HTML export"
	@echo "Dependencies installed."

.PHONY: test
test:
	@echo "=== Testing R scripts ==="
	@$(R) -e "cat('R is working!\n')"
	@echo "=== Checking if source files exist ==="
	@test -f "$(SRC_SPECTRAL)" && echo "✓ $(SRC_SPECTRAL)" || echo "✗ $(SRC_SPECTRAL) NOT FOUND"
	@test -f "$(SRC_GAP)" && echo "✓ $(SRC_GAP)" || echo "✗ $(SRC_GAP) NOT FOUND"

.PHONY: help
help:
	@echo "Spectral Clustering Project Makefile"
	@echo "===================================="
	@echo ""
	@echo "Available targets:"
	@echo "  make all           - Run complete analysis pipeline"
	@echo "  make install-deps  - Install required R packages"
	@echo "  make setup         - Create output directories"
	@echo "  make spectral      - Run spectral clustering analysis"
	@echo "  make gap           - Run gap statistic analysis"
	@echo "  make report        - Generate summary report"
	@echo "  make test          - Test setup"
	@echo "  make clean         - Remove intermediate files"
	@echo "  make clean-all     - Remove all output files"
	@echo "  make help          - Show this help"

.PHONY: all setup spectral gap report clean clean-all install-deps test help