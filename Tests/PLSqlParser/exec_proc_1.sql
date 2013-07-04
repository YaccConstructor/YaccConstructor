begin
    dbms_network_acl_admin.drop_acl(
      acl =>         'PL-jrxml2pdf_google_maps.xml'
    );
end