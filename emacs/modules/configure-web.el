(when (getenv "WSLENV")
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
	(cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
	    browse-url-generic-args     cmd-args
	    browse-url-browser-function 'browse-url-generic
	    search-web-default-browser 'browse-url-generic))))

(use-package elfeed
  :bind (:map elfeed-search-mode-map
	      ("C-<return>" . elfeed-search-browse-url))
  :config
  (setq elfeed-db-directory (dv/get-cache-path "elfeed/"))
  (setq elfeed-search-date-format '("%d %b %Y" 11 :left))
  (setq elfeed-search-filter "@6-months-ago")
  (setq elfeed-feeds
	'("https://jvns.ca/atom.xml"
	  "https://fasterthanli.me/index.xml"
	  "http://antirez.com/rss"
	  "https://nullprogram.com/feed/"
	  ;; "http://www.johndcook.com/blog/feed/"
	  "https://blog.codinghorror.com/rss/"
	  "https://www.joshwcomeau.com/rss.xml"
	  "https://www.allthingsdistributed.com/atom.xml"
	  "https://waitbutwhy.com/feed"
	  "https://dcgross.com/feed.xml" ;; Use: https://web.archive.org/web/sitemap/dcgross.com
	  "https://paulgraham.com/rss.html"
	  "https://mtlynch.io/posts/index.xml"
	  "https://www.joelonsoftware.com/feed/"
	  "https://eli.thegreenplace.net/feeds/all.atom.xml"
	  "https://www.swyx.io/rss.xml"
	  "https://coderscat.com/atom.xml"
	  "https://www.kalzumeus.com/feed/articles/"
	  ;; "https://daringfireball.net/feeds/main"
	  )))

(provide 'configure-web)
