Papertool
=========

Papertool is (will be) a multi-datasource **client for viewing and navigating
scientific publications** written in Rust. It allows to **navigate** through
the **references** and citations of a publication, query metadata and even
retrieve a (link to) the PDF file in most cases.

It's **extensible** and can support multiple data sources. It can deal with
wrong metadata, ultimately by asking the user on how to fix e.g. a situation
where two publications have the same DOI (which cannot be right, DOIs are
unique).

All queried data is locally **cached** in a SQLite3 database to speed up
subsequent accesses.

This is still in an early stage of development. The "hard stuff" seems to work
quite well already, though this is certainly not usable yet for real-world
usage.

Data sources are currently implemented or planned:

  - SemanticScholar for citations/references and PDF link (implemented)
  - DBLP for metadata (planned)
  - CrossRef for metadata (planned)

The philosophy is to use as raw as possible data from the sources, and to
cook them into nice BibTeX records using heuristics afterwards. This has the
advantage that changes in heuristics can easily be implemented and affect
*all* data, not just those retrieved after the change.

Usage
-----

You can run the tool by just typing `cargo run` in a terminal.

This will currently query all papers that cite at least one of 5 publications I
deemed "interesting" (hardcoded DOIs in main.rs), sort them by their count of
references to "interesting" papers. The top few of that list show a link to a
PDF version of that paper by using an experimental semanticscholar feature.

Results are cached to `/tmp/bla.sqlite` for the moment. If any weird panics
occur, delete that cache file.

Note that the example query will fail two times, because of conflicting DOIs, with
the following message:

```
##########################
# NEED USER INTERVENTION #
##########################
Got #1 from API:
'Configurable 3D Scene Synthesis and 2D Image Rendering with Per-pixel Ground Truth Using Stochastic Grammars' by Chenfanfu Jiang et al. (doi:10.1007/s11263-018-1103-5, semanticscholar:d24ff1cde1673ba1ecb4890dd7eeaa85d464221c)
Got #2 from database due to same doi:
'Configurable, Photorealistic Image Rendering and Ground Truth Synthesis by Sampling Stochastic Grammars Representing Indoor Scenes' by Chenfanfu Jiang et al. (doi:10.1007/s11263-018-1103-5, arxiv:1704.00112, semanticscholar:6597649cf4b45fac4ae62d455d271c1ce9e829f3)

Obviously, one of these doi fields is wrong. Please enter a correction using the following syntax:
        > doi1=<insert new doi here>           to change #1's doi to something else or
        > arxiv2=-,doi2=-                      to change #2's arxiv id and doi to <none>
        > 1=<...>                              to change #1's id, if there is only one conflicting id
> 
```

This is because SemanticScholar wrongly gives the same DOI for two different
papers. Resolve this conflict e.g. by typing `2=` (or `doi2=-`) to permanently
and persistently clear #2's doi entry.

Then re-run the program, and another similar error will be shown. After
handling that one, in the third run, the programm will successfully retrieve
all data and show:

```
...
    (
        "Automatic Generation of Vivid LEGO Architectural Sculptures",
        "didn\'t bother",
        1,
    ),
    (
        "Attribute And-Or Grammar for Joint Parsing of Human Attributes, Part and Pose",
        "didn\'t bother",
        1,
    ),
    (
        "Partial Procedural Geometric Model Fitting for Point Clouds",
        "https://arxiv.org/pdf/1610.04936.pdf",
        2,
    ),
    (
        "Guided Proceduralization: Optimizing Geometry Processing and Grammar Extraction for Architectural Models",
        "https://arxiv.org/pdf/1807.02578.pdf",
        2,
    ),
    (
        "3D Semantic Segmentation of Modular Furniture Using rjMCMC",
        "http://web-info8.informatik.rwth-aachen.de/media/papers/egpaper_final_GX7r76o.pdf",
        2,
    ),
    (
        "Neural Procedural Reconstruction for Residential Buildings",
        "http://openaccess.thecvf.com/content_ECCV_2018/papers/Huayi_Zeng_Neural_Procedural_Reconstruction_ECCV_2018_paper.pdf",
        2,
    ),
    (
        "A Generalized Proceduralization Framework for Urban Models with Applications in Procedural Modeling, Synthesis, and Reconstruction",
        "no pdf",
        2,
    ),
```
