<?xml version="1.0" encoding="utf-8" ?>
<stylesheet
    xmlns="http://www.w3.org/1999/XSL/Transform"
    version="1.0">
  <strip-space elements="*" />
  <template match="para[content-style][not(text())]">
    <value-of select="normalize-space(.)" />
  </template>
  <template match="node()|@*">
    <copy><apply-templates select="node()|@*" /></copy>
  </template>
  <output indent="yes"
          method="xml"
          omit-xml-declaration="yes"
          />
</stylesheet>
