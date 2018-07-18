find . -type f -name "*.fsproj" -exec sed -i -- 's/net46/netstandard2.0/g' {} +
find . -type f -name "*.csproj" -exec sed -i -- 's/net46/netstandard2.0/g' {} +