RStudio Addin to Post to Qiita
==============================

*Caution: This package is in the verrrrry early stages.*

### Installation

I guess you cannot install this now because of some error during installation of shinygadgets. Wait for the time to come.

```r
library(devtools)
install_github("yutannihilation/qiitaddin")
```

### Usage

After making sure that the target R Markdown file is active, click "Post to Qiita".

![](usage1.png)

Review the knitted Markdown document and click "Done". Then, provide your Qiita access token to another dialogue.

![](usage2.png)

After that, the document is quietly(this needs to be inproved) posted to Qiita as a private post.

There are two important things you have to be careful about:

* Since no method to upload images is provided by Qiita API, you have to upload images by hand.
* Qiita is more formal place than Rpubs; Qiita people may get mad at you if you publish some test document that is not for sharing your knowledge, but for just testing Qiita API and this package. Never make it public.
