namespace Day_2
{
    class Program
    {

        static int Dive(string fichero)
        {
            int i = 0;
            int answer = 0;
            int horizontalPosition = 0;
            int depth = 0;
            int aim = 0;

            int lineCounter = 0;
            using (var reader = File.OpenText(fichero))
            {
                while (reader.ReadLine() != null)
                {
                    lineCounter = lineCounter + 1;
                }
            }

            while (i <= lineCounter - 1)
            {
                string lineA = File.ReadLines(fichero).Skip(i).Take(1).First();

                string number = lineA.Substring(lineA.Length - 1);
                int num1 = Convert.ToInt32(number);

                string direction = lineA.Substring(0, lineA.Length - 2);

                if (direction == "up")
                    aim = aim - num1;
                else if (direction == "down")
                    aim = aim + num1;
                else if (direction == "forward")
                {
                    horizontalPosition = horizontalPosition + num1;
                    depth = depth + num1 * aim;
                }

                i = i + 1;
            }

            answer = depth * horizontalPosition;

            return answer;
        }

        static void Main(string[] args)
        {
            string file;
            Console.WriteLine("Please write the input (.txt) file path");
            file = Console.ReadLine();
            System.Console.WriteLine("The result is " + Dive(file));


        }
    }
}