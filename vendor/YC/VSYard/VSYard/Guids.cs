// Guids.cs
// MUST match guids.h
using System;

namespace YC.VSYard
{
    static class GuidList
    {
        public const string guidVSYardPkgString = "fddebfe9-87ff-4040-a646-f1f90015744c";
        public const string guidVSYardCmdSetString = "d990610f-204a-44a6-8697-5efb24d0ecb7";

        public static readonly Guid guidVSYardCmdSet = new Guid(guidVSYardCmdSetString);
    };
}