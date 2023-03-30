#pragma once

namespace lumen
{
	enum class TokenType : uint16
	{
		#define TOKEN(X) X
		#include "TokenDef.h"
		Count
	};

}