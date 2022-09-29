import { ToolButtonProps } from "./types";

export const ToolButton = ({ onClick, children, style }: ToolButtonProps) => {
    const [hover,setHover] = useState(false);
    return <div
        onClick={onClick}
        onMouseEnter={() => setHover(true)}
        onMouseOut={() => setHover(false)}
        style={{
            width: 30,
            height: 30,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            cursor: 'pointer',
            backgroundColor: hover ? 'rgb(50,100,200)' : 'rgb(61, 113, 217)',
            fontWeight: 700,
            borderRadius: 4,
            ...style
        }}>
        {children}
    </div>
}
