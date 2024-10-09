TARFILE := $(wildcard Code*.tar.gz)
all:
	tar -xf $(TARFILE)
	mkdir -p plot/out
	cd plot && Rscript param-estimator.R
	cd plot && Rscript param-estimator-2d.R
	cd plot && Rscript permanogva.R
	cd plot && Rscript model-selection.R
	cd plot && Rscript k-medoids.R
	cd plot && Rscript search-comparison.R
	cd plot && Rscript time.R

clean:  
	rm -rf Code
	rm -f plot/out/*

