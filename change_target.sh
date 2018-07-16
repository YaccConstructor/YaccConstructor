find . -type f -name "*.fsproj" -exec sed -i -- 's/netstandard2.0/net45/g' {} +
find . -type f -name "*.csproj" -exec sed -i -- 's/netstandard2.0/net45/g' {} +