using System;

namespace RudimentaryInterpreter
{
    internal abstract class Expression
    {
        protected object Value { get; private set; }

        public Expression(object value) => Value = value;
    }

    internal class Number : Expression
    {
        public Number(object value) : base(value) { }

        public override string ToString() => Value.ToString();
    }

    internal class Addition : Expression
    {
        public Addition(object value) : base(value) { }
    }

    internal class Subtraction : Expression
    {
        public Subtraction(object value) : base(value) { }
    }

    internal class Multiplication : Expression
    {
        public Multiplication(object value) : base(value) { }
    }

    internal class Division : Expression
    {
        public Division(object value) : base(value) { }
    }

    internal class Substitution : Expression
    {
        public Substitution(object value) : base(value) { }
    }

    internal class Symbol : Expression
    {
        public Symbol(object value) : base(value) { }
    }
}