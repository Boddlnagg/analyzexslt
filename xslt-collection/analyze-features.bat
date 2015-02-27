REM --- This batch file analyzes all relevant files from the xslt-collection with appropriate (tested) parameters ---

cd ..

call sbt --error "set showSuccess := false" "run -f --csv xslt-collection/00-musicxml/stylesheet.xsl xslt-collection/01-libreoffice-spreadsheetml/stylesheet.xsl xslt-collection/02-svg-example/stylesheet.xsl xslt-collection/03-nunitreport/stylesheet.xsl xslt-collection/04-sick-of-beige-svg/stylesheet.xsl xslt-collection/05-pocket2keepass/stylesheet.xsl xslt-collection/06-docbook-html5/stylesheet.xsl xslt-collection/07-ead2002-to-wordml/stylesheet.xsl xslt-collection/08-epub-nav/stylesheet.xsl xslt-collection/09-zefania2html/stylesheet.xsl xslt-collection/10-dc2mods/stylesheet.xsl xslt-collection/11-sphinxxml2review/stylesheet.xsl xslt-collection/12-umbraco-mediahelper/stylesheet.xsl xslt-collection/13-brainfuck/stylesheet.xsl xslt-collection/14-iati-activities-rdf/stylesheet.xsl xslt-collection/15-github2html/stylesheet.xsl" > xslt-collection/features.csv

pause