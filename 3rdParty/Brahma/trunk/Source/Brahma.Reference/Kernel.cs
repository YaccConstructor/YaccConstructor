using System.Collections.Generic;
using System.Linq.Expressions;
using System.Text;
using Brahma.Types;

namespace Brahma.Reference
{
    internal interface ICSKernel : IKernel
    {
        StringBuilder Source
        {
            get;
        }

        int32 WorkDim
        {
            get;
        }

        void SetClosures(IEnumerable<MemberExpression> closures);
        void SetParameters(IEnumerable<ParameterExpression> parameters);
    }
}
