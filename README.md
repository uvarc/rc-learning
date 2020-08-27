i[![Build Status](https://travis-ci.com/uvarc/rc-website.svg?branch=master)](https://travis-ci.com/uvarc/rc-website)

# UVA Research Learning & Workshop Website

| Staging URL   | Production URL |
| ------------- | ------------- |
| https://staging.learning.rc.virginia.edu | https://www.learning.rc.virginia.edu  |

  * [Developing](#developing)
     * [Using Gitpod](#using-gitpod)
     * [Local Install](#local-install)
  * [Creating New Content](#creating-new-content)
     * [Methods for creating content](#two-methods-for-creating-content)
     * [Suggestions for creating content](#helpful-notes-about-creating-content)
     * [Front matter](#front-matter)
     * [Future Posts](#future-posts)
     * [Shortcodes](#shortcodes)
     * [Featured Content](#featured-content)
     * [Preview content locally](#preview-content-locally)
     * [Publish content](#publish-content)
     * [Delete content](#delete-content)
     * [Events Data](#events-data)
     * [Automated Builds](#automated-builds)
  * [Search](#search)

## Developing

### Local Install

* [Install](https://gohugo.io/overview/installing/) the HUGO binary on your local computer. This has been tested with version 0.70.x-extended. **You need the extended version.** For more information, see the Hugo GitHub repo: https://github.com/spf13/hugo.
* Clone this website repository: `git clone git@github.com:uvarc/rc-learning.git`.  The `master` branch is protected.  New content has to be pushed to the `staging` branch. Use this command to clone the staging branch only: `git clone --single-branch -b staging git@github.com:uvarc/rc-learning.git`.

- - -

## Creating New Content

The `TL;DR` version:

1. Make your changes to the `staging` branch and be sure to preview locally before you push back to GitHub.
2. All website pages are stored within `/content/`
3. You can use Markdown or HTML (or a mix of both) within pages.

Content of this website is contained in a series of markdown files within the `content/` subdirectory. The site hierarchy consists of 7 subsections:

* `authors` - Mission statement and staff directory.
* `courses` - short courses with multi-chapter content, e.g. Summer Education Series
* `notes` - Contains all markdown files for workshops, courses, or tutorials
* `slides` - Markdown doc will be rendered as slides
* `tutorials` - self-paced material not associated with particular workshop.
* `videos` - unused at the moment.
* `workshops` - Systems and information we support: Rivanna, Ivy, Skyline, etc., and detailed user information.

### Methods for creating content:

1. Copy an existing page and modify it.
2. Create a new page using the `hugo new` command declaring a path to the .md object you want to create:
  * `hugo new post/here-is-my-post.md`
  * `hugo new top-level-page.md`
  
### Suggestions for creating content:

  * The "content type" of a page is usually determined by what folder it is in. Different content types are displayed in slightly different ways, i.e. the sidebar or layout.
  * Reference: [markdown cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)
  * Store images in `static/images/`. For example, the published URL of `static/images/uva-logo.jpg` will be https://www.rc.virginia.edu/images/uva-logo.jpg
  * Follow guidelines from https://gohugo.io/content/organization/.

### Front matter

Metadata for each web page is contained in YAML format at the top of each markdown page. The only required fields are usually TITLE and DATE. Categories and Tags can be as numerous as you find useful.

title: my workshop
draft: false
type: article
toc: true

### Future Posts

Using the `date` metadata smartly, you can forward date any post or article. [Automated builds](#automated-builds) happen each morning and your page will be published when that datetime has passed.

### Shortcodes


### Preview content locally
`hugo server` will bring up the local hugo server and give you a preview URL `http://localhost:1313/`. If making many changes, open another terminal to keep the `hugo server` running as you edit.

### Publish content
Simply push `staging` back to GitHub. We're in the process of setting up the S3 buckets for hosting the content. 

TravisCI will handle it from there and publish the content at the staging site, https://staging.learning.rc.virginia.edu. Pushing your content to the production website requires a PULL REQUEST.

> Remember that after pushing your changes back to the `staging` branch, the https://staging.rc.virginia.edu/ website will be updated within 1-2 minutes. Hold down the SHIFT key when reloading your browser to refresh your local cache.

### Delete content
* Delete the .md object(s) you no longer want in the site, then commit and push.
* To temporarily remove content, set the `draft` status of any .md object to `true`.
* Republishing deletes remote files in S3/CloudFront.

### Automated Builds

Travis-CI is a CI/CD tool that automates builds and deployments of the website code. Take note of the contents of `.travis.yml` and you will see instructions for how Travis builds the site:
* Upon a push to `staging` or `master` it launches a customized container `uvarc/hugo-build:v2`.
* That container runs a script that clones that branch of the repository and runs `hugo -v --ignoreCache` to build the site.
* Travis then synchronizes the published HTML, JS, CSS, images and files to Amazon S3.
* Finally, the build invalidates the CloudFront cache that serves out the actual website.

Build+deployment generally takes 70 seconds and can be monitored using the [Travis-CI dashboard](https://travis-ci.com/uvarc/rc-website/builds) for this repository.

