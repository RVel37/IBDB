## IBDB data

Code used to process the data seen in IBDB is available in
the IBDB <a href='https://github.com/RVel37/IBDB' target='_blank'>GitHub repository</a>.


Data are stored on a publicly-accessible AWS bucket and can be downloaded in bulk
via the following command (assumes you have AWS CLI installed):

```shell
aws s3 cp s3://ibddb-data.s3.us-west-2.amazonaws.com/ ibdb-data/
```


## Download Links
- [file_name.zip](https://www.fake-link.com)
- [file_name.zip](https://www.fake-link.com)
- [file_name.zip](https://www.fake-link.com)
- [file_name.zip](https://www.fake-link.com)


---

<details>
<summary><strong>Data details (click to unfold)</strong></summary>

<br>

* **metadata.csv**
  - A CSV file detailing the samples in the dataset
  - Structure:
    * *sample_id*
      - The ID of the sample, in SRA run accession format
    * *study_id*
      - The GEO ID for the study from which data were derived
    * *accession*
      - The GEO accession number of the sample
    * *condition*
      - The biological condition of the sample
    * *paired_end*
      - A logical indicating whether the data are paired-end
    * *stranded*
      - A string indicating the strandedness of each sample
* **contrasts.csv**
  - A CSV file detailing the contrasts used in calculating DEGs
  - Structure:
    * *study_id*
      - The GEO ID for the study from which data were derived
    * *numerator*
      - In DGE analysis, the numerator
    * *denominator*
      - In DGE analysis, the denominator
* **GSE126848_degs.csv.gz** and **GSE135251_degs.csv.gz**
  - GZ-compressed CSV files containing the DEG results for comparison from *contrasts.csv*
  - Structure:
    * *gene_id*
      - Ensembl gene ID
    * *numerator*
      - The condition of samples used as the numerator in the DEG
    * *denominator*
      - The condition of samples used as the denominator in the DEG
    * *logFC*
      - The fold change of gene expression between the numerator and denominator (see *contrasts.csv*)
    * *FDR*
      - The significance of the differential gene expression, with multiple testing correction
* **GSE126848_gene_exp.csv.gz** and **GSE135251_gene_exp.csv.gz**
  - GZ-compressed CSV files containing the expression levels for each gene within each sample.
  - Structure:
    * *gene_id*
      - Ensembl gene ID
    * *sample_id*
      - The ID of the sample, in SRA run accession format
    * *cpm*
      - The normalized 'Counts Per Million' as derived from edgeR
    * *rpkm*
      - The 'Reads per Kilobase of transcript, per Million mapped reads'
    * *tpm*
      - The 'Transcripts Per Million'
* **enrichr_res.csv.gz**
  - A GZ-compressed CSV file containing the significant KEGG pathway enrichment results from
    <a href='https://cran.r-project.org/web/packages/enrichR/vignettes/enrichR.html' target='_blank'>enrichR</a>
  - Structure
    * *Study_Contrast*
      - Study ID and contrast of the enrichR results
    * *Term*
      - KEGG pathway analyzed
    * *Overlap*
      - Proportion of genes from tested
    * *P.value*
      - P value from enrichment test
    * *Adjusted.P.value*
      - P value adjusted for multiple testing
    * *Old.P.value*
      - P value from enrichment test (from an older method of calculation)
    * *Old.Adjusted.P.value*
      - P value adjusted for multiple testing (from an older method of calculation)
    * *Odds.Ratio*
      - Odds ratio from Fisher's exact test
    * *Combined.Score*
      - Aggregate score derived from odds ratio and estimated Z score. See
        <a href='https://maayanlab.cloud/Enrichr/help#background&q=4' target='_blank'>enrichr documentation</a>
        for more details
    * *Genes*
      - Genes from query dataset that were also found in tested gene set
    * *group*
      - DEG type on which enrichment was calculated (over-expressed or under-expressed)
</details>
