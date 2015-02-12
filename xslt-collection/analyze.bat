REM --- This batch file analyzes all relevant files from the xslt-collection with appropriate (tested) parameters ---

cd ..

call sbt --error "set showSuccess := false" "run -p -r 10 xslt-collection/00-musicxml/reduced.xsl" > xslt-collection/00-musicxml/recursion-limit-10.txt
call sbt --error "set showSuccess := false" "run -pb -r 8 xslt-collection/02-svg-example/reduced.xsl" > xslt-collection/02-svg-example/recursion-limit-8.txt
call sbt --error "set showSuccess := false" "run -pb -r 9 xslt-collection/02-svg-example/reduced.xsl" > xslt-collection/02-svg-example/recursion-limit-9.txt
call sbt --error "set showSuccess := false" "run -pb -r 10 xslt-collection/02-svg-example/reduced.xsl" > xslt-collection/02-svg-example/recursion-limit-10.txt
call sbt --error "set showSuccess := false" "run -p -r 10 xslt-collection/03-nunitreport/reduced.xsl" > xslt-collection/03-nunitreport/recursion-limit-10.txt
call sbt --error "set showSuccess := false" "run -p xslt-collection/04-sick-of-beige-svg/reduced.xsl" > xslt-collection/04-sick-of-beige-svg/recursion-limit-none.txt
call sbt --error "set showSuccess := false" "run -p -r 10 xslt-collection/05-pocket2keepass/stylesheet.xsl" > xslt-collection/05-pocket2keepass/recursion-limit-10.txt
call sbt --error "set showSuccess := false" "run -p -r 6 xslt-collection/07-ead2002-to-wordml/reduced.xsl" > xslt-collection/07-ead2002-to-wordml/recursion-limit-6.txt
call sbt --error "set showSuccess := false" "run -p -r 7 xslt-collection/07-ead2002-to-wordml/reduced.xsl" > xslt-collection/07-ead2002-to-wordml/recursion-limit-7.txt
call sbt --error "set showSuccess := false" "run -p -r 10 xslt-collection/08-epub-nav/reduced.xsl" > xslt-collection/08-epub-nav/recursion-limit-10.txt
call sbt --error "set showSuccess := false" "run -p xslt-collection/08-epub-nav/nobuiltin.xsl" > xslt-collection/08-epub-nav/nobuiltin-recursion-limit-none.txt
call sbt --error "set showSuccess := false" "run -p -r 10 xslt-collection/09-zefania2html/stylesheet.xsl" > xslt-collection/09-zefania2html/recursion-limit-10.txt
call sbt --error "set showSuccess := false" "run -p xslt-collection/09-zefania2html/nobuiltin.xsl" > xslt-collection/09-zefania2html/nobuiltin-recursion-limit-none.txt
call sbt --error "set showSuccess := false" "run -p -r 10 xslt-collection/11-sphinxxml2review/reduced.xsl" > xslt-collection/11-sphinxxml2review/recursion-limit-10.txt
call sbt --error "set showSuccess := false" "run -p xslt-collection/14-iati-activities-rdf/reduced.xsl" > xslt-collection/14-iati-activities-rdf/recursion-limit-none.txt
call sbt --error "set showSuccess := false" "run -pb -r 5 xslt-collection/15-github2html/reduced.xsl" > xslt-collection/15-github2html/recursion-limit-5.txt