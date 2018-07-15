find . -type f -name "*.fsproj" -exec sed -i -- 's/netstandard2.0/netcoreapp2.1/g' {} +
find . -type f -name "*.csproj" -exec sed -i -- 's/netstandard2.0/netcoreapp2.1/g' {} +