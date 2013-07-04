begin
  begin
    dbms_network_acl_admin.drop_acl(
      acl =>         'PL-jrxml2pdf_google_maps.xml'
    );
  exception 
    when others then null; -- ACL does not exist yet
  end;
  -- Privilege to connect to a host
  dbms_network_acl_admin.create_acl(
    acl =>         'PL-jrxml2pdf_google_maps.xml',
    description => 'HTTP-Connects to http://maps.google.com',
    principal =>   '&SCHEMA', -- DB Schema (grantee)
    is_grant =>    true,
    privilege =>   'connect',
    start_date  => null, 
    end_date  =>   null
  );
  -- Privilege to resolve a hostname (DNS lookup)
  DBMS_NETWORK_ACL_ADMIN.ADD_PRIVILEGE(
    acl =>         'PL-jrxml2pdf_google_maps.xml',
    principal =>   '&SCHEMA', -- DB Schema (grantee)
    is_grant  =>   true,
    privilege =>   'resolve',
    start_date  => null, 
    end_date  =>   null
  );
  -- Privilege to connect to www.oracle.de
  dbms_network_acl_admin.assign_acl(
    acl =>         'PL-jrxml2pdf_google_maps.xml',
    host =>        'maps.google.com',
    lower_port =>  80,
    upper_port =>  80
  );
  -- Privilege to connect to www.oracle.com (oracle.de is redirected to oracle.com)
  dbms_network_acl_admin.assign_acl(
    acl =>         'PL-jrxml2pdf_google_maps.xml',
    host =>        'maps.google.com',
    lower_port =>  80,
    upper_port =>  80
  );
end;
/
    
commit
/